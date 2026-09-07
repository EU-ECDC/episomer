# Multi-instance runners — manual test plan (issues 1–8)

Companion to `inst/schema/multi_services_plan.md`. It covers what the unit tests deliberately
cannot: the lease going through the **fs API over HTTP** (only the `fs_remote = NULL` branch is
unit-tested), real **multi-process** behaviour, real **crashes**, and the **Shiny** rendering.

Every step ends on something you can *see* — a file in `EPI_HOME`, an mtime, a log line, a table
row — never on "it seems to work".

IDs are stable (`M1-1`, `M2-3`, …): quote them in issues and in commit messages.

---

## 0. Before anything

```bash
export EPI_HOME=/home/yohann_mansiaux/EPI_HOME     # already in .Renviron
cd ~/projects/episomer
```

**M0-1 — the unit suite is green.** Nothing below is worth debugging on top of a red suite.

```bash
Rscript -e 'devtools::test("episomer", filter = "runners|config_runner|fs|paths")'
```

- [x] 0 failures.

**M0-2 — the observation toolkit.** Keep this running in a dedicated terminal for the whole
session; it is the single most useful window of this plan.

```bash
watch -n 2 '
echo "--- live leases ---";  ls -l --time-style=+%H:%M:%S $EPI_HOME/runners/*.json 2>/dev/null;
echo "--- released ---";     ls -l --time-style=+%H:%M:%S $EPI_HOME/runners/old/  2>/dev/null;
echo "--- PID files ---";    ls -l --time-style=+%H:%M:%S $EPI_HOME/*.PID         2>/dev/null'
```

Dump one lease with its mtime — the two are *not* the same information (§3.2: freshness comes from
the mtime, `renewed_at` only moves when the content is rewritten):

```bash
for f in $EPI_HOME/runners/*.json; do
  echo "== $(basename $f)  mtime=$(stat -c %y "$f")"; jq -c . "$f"; done
```

The lease helpers are internal, so from an R console they need `episomer:::`:

```r
library(episomer); setup_config()
episomer:::runner_leases_df()                        # exactly what the Shiny table shows
episomer:::runner_lease_status("search.bluesky")
episomer:::conf$instance_id; episomer:::conf$instance_host
```

**Runner names you should expect to see**, and their TTL:

| Runner | Written by | TTL |
|---|---|---|
| `fs` | `fs_loop()`, beaten by the fs monitor | 120 s |
| `fs_mon` | the fs monitor future, every 2 s | 120 s |
| `search` | `search_loop()` (parent) | 300 s |
| `search.bluesky`, `search.mastodon.<host>` | `search_loop_worker()` | 300 s |
| `detect` | `detect_loop()` | 1800 s |

Logs: `$EPI_HOME/jobs/{fs,search,detect}.log`, one file per network in
`$EPI_HOME/jobs/search.<network>.log`, plus `docker compose -f docker/compose.yml logs <service>`.

---

## 1. Identity (issue 1)

**M1-1 — `instance_id` is stable within a process, `instance_host` comes from the environment.**

```bash
EPISOMER_INSTANCE=machine-A Rscript -e '
  library(episomer); setup_config(); a <- episomer:::conf$instance_id
  setup_config();                    b <- episomer:::conf$instance_id
  cat("same id:", identical(a, b), "| id:", a, "| host:", episomer:::conf$instance_host, "\n")'
```

- [x] `same id: TRUE` — a second `setup_config()` must not hand out a new identity, otherwise a
      running loop would lose the lease it legitimately holds.
- [x] `host: machine-A`.
- [x] Without `EPISOMER_INSTANCE`, the host falls back to `hostname`.

**M1-2 — the identity is never persisted.** This one is the whole point: an `instance_id` shared
through `properties.json` would make two machines look like the same instance. `instance_id|instance_host|runner_name` are absent from
`properties.json`

```bash
grep -E 'instance_id|instance_host|runner_name' $EPI_HOME/properties.json
```

- [x] no match. Re-run after the Shiny app has saved the configuration once (it calls
      `save_config()`), and check again — that is the path that could reintroduce it.

**M1-3 — the containers are named.**

```bash
docker compose -f docker/compose.yml config | grep EPISOMER_INSTANCE
```

- [x] three distinct values: `fs`, `search`, `detect`.

---

## 2. The lease file appears and beats (issues 2, 4, 5)

**M2-1 — a clean start creates one file per runner.**

Start from an empty `runners/` (only when nothing is running):

compose up ne marche pas

```bash
make compose-down
rm -rf $EPI_HOME/runners
make compose-up
```

After ~1 minute:

```bash
ls $EPI_HOME/runners/
```

- [x] `fs.json`, `fs_mon.json`, `search.json`, `search.bluesky.json`,
      `search.mastodon.<host>.json`, `detect.json`.
- [x] The folder was created by the *writers*, not by every reader: `runners/old/` does **not**
      exist yet at this point.

```bash
jq . $EPI_HOME/runners/detect.json
```

- [ ] fields `runner`, `service`, `instance_id`, `host`, `pid`, `episomer_version`, `claimed_at`,
      `renewed_at` — and **no `released` field** (§4: a released lease is a *moved* file).
- [ ] `service` is the family, not the full name: `search.bluesky` → `"service": "search"`.
- [ ] `host` matches `EPISOMER_INSTANCE` of the container that wrote it (`detect`, `search`, `fs`).
- [ ] `pid` is the PID *inside* that container — cross-check with
      `docker compose -f docker/compose.yml exec detect cat /episomer-data/detect.PID`.

This whole step already proves the HTTP path: the `search` and `detect` containers do not mount
`$EPI_HOME`, they only reach the fs over `EPI_HOST`. The files you are reading were written through
`POST /file/...?action=upload`.

**M2-2 — the heartbeat moves the mtime and leaves the content alone.**

```bash
f=$EPI_HOME/runners/search.bluesky.json
stat -c %y $f; md5sum $f; jq -r .renewed_at $f
sleep 90
stat -c %y $f; md5sum $f; jq -r .renewed_at $f
```

- [ ] mtime advanced, and by no more than ~`runner_heartbeat_span` (30 s).
- [ ] **md5 unchanged** and `renewed_at` unchanged — renewing is `epifile.touch()`, one tiny POST
      with no body (§3.2). A changing md5 here means something is rewriting the lease on every
      beat, which is a regression.
- [ ] same for `fs.json` (beaten by the fs monitor on behalf of a process that never returns to R)
      and `fs_mon.json`.

**M2-3 — the parent `search` lease goes stale on a healthy machine.** Documented, not a bug
(§9.5): `search_loop()` blocks in `parLapply()` and has no iteration left to beat on.

```bash
stat -c %y $EPI_HOME/runners/search.json      # frozen at claim time
stat -c %y $EPI_HOME/runners/search.bluesky.json  # moving
```

- [ ] after 5 minutes, `search.json`'s mtime has not moved while `search.bluesky.json`'s has.
- [ ] and yet the Shiny app still says the search service is **Running** (M6-2) — that is exactly
      the workaround issue 7 put in `is_search_running()`.

**M2-4 — a long sleep does not look like death.** The search loop sleeps until its next collection
slot, up to `collect_span` (60 min) against a 300 s TTL.

```bash
tail -f $EPI_HOME/jobs/search.bluesky.log      # find a "waiting"/idle stretch
```

- [ ] during a wait of several minutes with no API request at all, `search.bluesky.json`'s mtime
      keeps advancing every ~30 s. Without `sleep_with_heartbeat()` it would freeze and the standby
      would steal the lease from a perfectly healthy runner.

**M2-5 — the rate-limit wait, the one silence `sleep_with_heartbeat()` cannot reach.** Hardest to
provoke; try it when a `429` shows up naturally, or by lowering the TTL
(`EPISOMER_OPTION_RUNNER_TTL_SEARCH=90`) so that a normal Mastodon window overshoots it.

- [ ] while `httr2` is waiting inside `req_perform()` (the log shows a retry / rate-limit line),
      the lease mtime still advances — `heartbeat_before_retry()` is doing the waiting itself.
- [ ] the lease is never lost during a rate-limit window.

---

## 3. Release on exit (issue 4, §3.4)

**M3-1 — a clean stop moves the lease to `runners/old/`.** Do this one **locally first**, where the
exit path is unambiguous:

```bash
make run-detect            # in terminal 1
# terminal 2:
cat $EPI_HOME/runners/detect.json
# terminal 1: Ctrl-C
```

- [ ] `$EPI_HOME/runners/detect.json` is **gone**.
- [ ] `$EPI_HOME/runners/old/detect.<instance_id>.json` exists, still readable, `<instance_id>`
      matching the one you read before stopping.

```bash
jq -r .instance_id $EPI_HOME/runners/old/detect.*.json
```

**M3-2 — the same on a container stop.** `docker compose stop` sends `SIGTERM`; whether R runs the
`on.exit()` handlers on that signal is precisely what this step is for.

```bash
docker compose -f docker/compose.yml stop detect
ls $EPI_HOME/runners/detect.json $EPI_HOME/runners/old/
```

- [x] **Answered, and the answer is no: it does not move.** R installs no `SIGTERM` handler, so an
      `Rscript` receiving one dies with every `on.exit()` skipped (and the container `ENTRYPOINT` is
      a `bash` script that does not forward the signal anyway, so `docker stop` reaches R as
      `SIGKILL` ten seconds later). This became **issue 13** of the plan: whoever issues the kill
      releases the lease, since the process being killed cannot. A `docker compose stop`/`down` runs
      no R code at all, so **that** path still waits out the TTL, by design.
- [ ] After a `docker compose stop detect`, `runners/detect.json` is therefore expected to stay.
      What must hold is the *next* line: it is expired, not eternal.

```bash
# 30 minutes later, or with EPISOMER_OPTION_RUNNER_TTL_DETECT lowered for the test
docker compose -f docker/compose.yml up -d detect
```

**M3-2b — a stop from the wizard, which is the path R controls.** This is where issue 13 has to be
verified: `stop_all_runner_tasks()` kills the processes *and* sweeps `runners/`.

```r
library(episomer)
setup(data_dir = "<EPI_HOME_LOCAL>")   # option 7 -> "Start all services", then again -> "Stop all"
```

- [ ] right after the stop, `<EPI_HOME_LOCAL>/runners/` holds **no `.json`** file...
- [ ] ...and `runners/old/` holds one per stopped runner, named after the **instance that held it**,
      not after the wizard's own instance:

```bash
for f in $EPI_HOME_LOCAL/runners/old/*.json; do echo "$f -> $(jq -r .instance_id $f)"; done
```

- [ ] starting the services again **immediately** works: `jobs/fs.log` says `Launching fs` and no
      `already held by another instance`. This is the regression to watch — it used to fail for a
      whole TTL, and with `fs.PID` deleted by that first failed attempt, for good.

**M3-3 — a release means an immediate reclaim, no TTL wait** (§7 case 4).

```bash
date; docker compose -f docker/compose.yml up -d detect; sleep 20
jq -r '.claimed_at, .instance_id' $EPI_HOME/runners/detect.json
```

- [ ] `claimed_at` is seconds old, `instance_id` is new. Restarting after a clean stop must not
      wait 30 minutes.

**M3-4 — `compose down` then `compose up`: the sweep happens on the way *up*, and it takes the PID
files with it.** A `down` runs no R code at all (M3-2), so both layers of every runner survive it
untouched. What cleans up is the startup of the `fs` container: `devscripts/run.R` calls
`setup(silent = TRUE)`, which picks option 1 and therefore `stop_all_runner_tasks()` →
`release_dead_local_runner_leases()`.

```bash
make compose-down
ls $EPI_HOME/runners/ $EPI_HOME/*.PID       # all still there, by design
make compose-up
docker compose -f docker/compose.yml logs fs | grep Released
```

- [ ] the log says `Released the lease of runner fs_mon: its holder (PID ...) is not running on this
      machine anymore`, and `runners/old/fs_mon.<instance_id>.json` names the run that just ended.
- [ ] `$EPI_HOME/fs_mon.PID` is **gone** right after that sweep — the release takes both layers, and
      that is the whole point of the step.
- [ ] a **new** `runners/fs_mon.json` appears within ~2 minutes, with a new `instance_id`, and
      `fs_mon.PID` names a process of the new container.
- [ ] `stat -c %y $EPI_HOME/runners/fs.json` keeps moving afterwards: the monitor is what beats for
      `fs` (M2-2), so a monitor that failed to register is also an `fs` lease going stale on a
      perfectly healthy service — and no `health_check()` either.

This is the regression to watch, and it used to fail exactly here: the sweep moved the lease but
left `fs_mon.PID` behind, PIDs are handed out from the same small range at every container start,
and one of the twenty-odd idle PSOCK workers of `future::plan(multisession)` had been given the
recorded PID back. `register_fs_monitor()` therefore refused ("is already running"), the monitor
future died on its first iteration, and nothing anywhere said so.

```bash
tail $EPI_HOME/fs_mon.log
```

- [ ] `Monitoring episomer` at startup, plus one line per failure the monitor survived. This file is
      the only place those can be read: `fs_loop()` never collects the value of the future, so
      everything the worker writes to stdout or raises as a condition stays in it forever.

**M3-4b — the same PID reuse, the other way round: `fs` is not released and waits its TTL out.**
Known gap, worth re-reading the fs log for what it costs:

```bash
docker compose -f docker/compose.yml logs fs | grep "already held\|has been claimed"
```

- [ ] `Runner 'fs' is already held by another instance ... waiting`, then `has been claimed after
      waiting 2 min`. The lease of the previous run names PID 9, and the main `Rscript` of the new
      container is PID 9 again, so `lease_holder_is_dead_locally()` sees a live process and refuses
      to release a lease whose holder is in fact long gone. Recording the *start time* of the holder
      process next to its PID is what would close this one; until then, a `compose up` after a
      `compose down` costs the fs one TTL.

---

## 4. Exclusivity (issue 4) — the core of the feature

**M4-1 — two instances on the same machine: the PID layer refuses.**

```bash
make run-detect            # terminal 1, leave it running
make run-detect            # terminal 2
```

- [ ] terminal 2 stops with `Runner  detect PID: <pid> is already running`.
- [ ] `$EPI_HOME/detect.PID` still holds terminal 1's PID.
- [ ] `$EPI_HOME/runners/detect.json` is untouched — same `instance_id`, same `claimed_at`.

**M4-2 — two instances on two "machines": the lease layer refuses.** This is the situation the
whole design exists for. Two `search` containers share the `fs` but each has its own
`/episomer-data`, so the PID layer cannot fire and only the lease can settle it.

```bash
docker compose -f docker/compose.yml up -d --scale search=2
docker compose -f docker/compose.yml logs --tail=80 search
```

- [ ] the second container logs, in substance:

      Runner 'search' is already held by another instance (<id>) running on host 'search'
      since <ts>, last seen N seconds ago (lease TTL: 300 seconds). This instance will not start it.

- [ ] the message names a **host** and a **claim time** — that is what makes it actionable.
- [ ] `$EPI_HOME/runners/search.json` still names the *first* instance: no file was overwritten,
      no second `search.bluesky.json` appeared.
- [ ] **the loser left no PID file behind** — the refusal has to roll the local layer back, or this
      machine would look like it hosts a running runner to `stop_all_runner_tasks()`:

```bash
docker compose -f docker/compose.yml exec --index 2 search ls -l /episomer-data/ | grep PID || echo "no PID file: correct"
```

Then bring it back to one: `docker compose -f docker/compose.yml up -d --scale search=1`.

**M4-3 — simultaneous start: exactly one wins** (§7 case 3, §3.1). Start both at once so the two
writes land within the claim guard:

```bash
make compose-down; rm -rf $EPI_HOME/runners
docker compose -f docker/compose.yml up -d --scale search=2
sleep 30; docker compose -f docker/compose.yml logs --tail=40 search
```

- [ ] one container collects, the other backs off with the holder message. Never both, never
      neither.
- [ ] `$EPI_HOME/runners/search.json` names exactly one `instance_id`, and it is the one whose
      container is still collecting.

**M4-4 — takeover after a hard death** (§7 case 5). No release, so the failover must cost one TTL.

```bash
docker compose -f docker/compose.yml kill -s KILL search
date; stat -c %y $EPI_HOME/runners/search.bluesky.json      # frozen from now on
```

- [ ] the mtime stops advancing immediately.
- [ ] after 300 s, the Shiny table row reads `stale (no heartbeat for 5 min)` (M6-1).
- [ ] before the 300 s are up, restarting (`up -d search`) is **refused** with the holder message —
      the standby must not steal a lease that could still be alive.
- [ ] after the 300 s, restarting **succeeds**: `instance_id` changes, `claimed_at` is fresh.

**M4-5 — the useful split: one machine per network** (§7 case 7). Nothing extra to code, so this is
purely a configuration check. Run two search instances, each with a single network activated:

```bash
EPISOMER_OPTION_SM_ACTIVATED_BLUESKY=TRUE  EPISOMER_OPTION_SM_ACTIVATED_MASTODON=FALSE ...   # machine B
EPISOMER_OPTION_SM_ACTIVATED_BLUESKY=FALSE EPISOMER_OPTION_SM_ACTIVATED_MASTODON=TRUE  ...   # machine C
```

- [ ] `search.bluesky.json` and `search.mastodon.<host>.json` name **different** `instance_id` /
      `host`, both fresh, both collecting.
- [ ] the parent `search` lease is held by only one of the two, and that does not stop the other
      one's worker (this is the §9.5 open question in its concrete form — write down what you see).

---

## 5. The guards (issue 6) — losing a lease mid-flight

Stealing a lease by hand is the cheapest way to reproduce the network-partition case (§7 case 6),
and `$EPI_HOME/runners/` is directly writable from the host since it *is* the fs volume.

**M5-1 — a search worker steps aside cleanly.** With the search loop running:

```bash
f=$EPI_HOME/runners/search.bluesky.json
jq '.instance_id="stolen-by-hand" | .host="machine-Z"' $f > /tmp/l.json && mv /tmp/l.json $f
tail -f $EPI_HOME/jobs/search.bluesky.log
```

- [ ] within ~30 s (the next heartbeat slice), two lines appear:

      ... [INFO]: -------> Runner lease 'search.bluesky' was taken by another instance, stopping
      ... [INFO]: -------> Another instance is now in charge of this runner: exiting cleanly and
                           leaving the lease to its new holder.

- [ ] **nothing was released**: `runners/search.bluesky.json` still says `stolen-by-hand`, and
      `runners/old/` gained **no** `search.bluesky.*` file. Releasing here would take the file away
      from the new holder — this is the single most important negative check of the plan.
- [ ] the worker exits without an R error traceback: it is a handover, not a crash.

**M5-2 — the parent loop explains itself.** Steal *every* network's lease and watch
`$EPI_HOME/jobs/search.log` (or `docker compose logs search`):

- [ ] `All search workers stopped because their lease was taken by another instance: bluesky,
      mastodon.<host> - this machine is not the search runner anymore, stopping the search loop.`
- [ ] the process ends **without** raising `All search loop proccesses ended, which is unexpected`.
      That historical error must stay reserved for a real, unexplained end — steal only one of two
      networks to see the other branch, which now names the networks and what each returned.

**M5-3 — the detect loop stops before writing.** With the detect loop running, steal `detect.json`
the same way.

- [ ] `$EPI_HOME/jobs/detect.log` shows the same two lines, and the loop exits.
- [ ] `$EPI_HOME/tasks.json` mtime **does not move** after the theft — no `save_tasks()`, no
      `generate_alerts()`, no alert email sent by a runner that is no longer in charge.
- [ ] `runners/detect.json` still names the thief.

**M5-4 — no posts are pushed after a takeover.** The guard in `search_topic()` sits *after* the
social media request, so a takeover happening during a slow request must still stop the upload.
Steal the lease while a request is in flight (easiest during a rate-limit wait):

- [ ] the log stops before the "uploading N posts" line, not after it.
- [ ] the post count on the fs side does not grow for that network after the theft.

**M5-5 — the Shiny app is not affected by a lost lease.** `save_tasks()` is deliberately unguarded
because the app calls it and holds no lease at all (§6.4). With `detect.json` stolen and the loop
gone:

- [ ] the app still lets you activate/deactivate a task, and `$EPI_HOME/tasks.json` mtime moves.
- [ ] no `episomer_lease_lost` error surfaces in the app.

**M5-6 — a real error is still a real error.** `run_until_lease_lost()` must not swallow anything
else. Provoke a genuine failure (e.g. stop the fs container under a running detect loop):

- [ ] the loop fails with the real error, **not** with the "another instance is now in charge"
      message.

---

## 6. What the Shiny app shows (issues 7 and 8)

```bash
make run-app          # on the host, with the services running in compose
```

Configuration page → the three service labels → the **Runners** table below them.

**M6-1 — the table.**

- [ ] five columns: `Runner | Status | Host | Last seen | Since`.
- [ ] one row per file in `$EPI_HOME/runners/*.json`, and **no row** for anything in
      `runners/old/` — a released runner has no row at all.
- [ ] reading order: `fs`, `fs_mon`, `search`, `detect`, then the `search.<network>` children.
- [ ] `Since` is UTC and does **not** move while `Last seen` keeps resetting: that pair is the
      picture of a healthy runner, and it is only readable together.
- [ ] no search box, no pagination, no sortable headers — it is a status block, and sorting would
      destroy the reading order.
- [ ] the app itself never appears as a runner (§6.4), and running two browser tabs adds no row.

**M6-2 — the three wordings of trouble.**

- [ ] healthy runner → `running`.
- [ ] `search` (the parent) after 5 minutes → `stale (no heartbeat for 5 min)`, while
      `search.bluesky` stays `running` and the service label above still says **Running** (M2-3).
- [ ] a runner killed on **this** machine, with its PID file still naming a dead process →
      `crashed here`, and it says so *immediately*, not after the TTL. Reproduce with `detect`,
      where the contrast is 30 minutes:

```bash
make run-detect                       # terminal 1
kill -9 $(cat $EPI_HOME/detect.PID)   # terminal 2
Rscript -e 'library(episomer); setup_config();
  s <- episomer:::runner_lease_status("detect");
  cat(s$status, s$holder_dead_locally, episomer:::is_detect_running(), "\n")'
```

- [ ] `stale TRUE FALSE` while the mtime is still fresh — and the app label flips to **Stopped**
      straight away.
- [ ] restarting `make run-detect` claims immediately, no TTL wait.

**M6-3 — the cross-check must *not* fire when it should not.** Three negative cases, all of which
would mean stealing a live lease if they were wrong:

- [ ] a live local process → stays `running` (obviously).
- [ ] `$EPI_HOME/detect.PID` deleted while the loop runs → the status falls back on the mtime,
      **not** on "the holder is dead". Without a corroborating PID file we know nothing.
- [ ] a lease whose `host` is another machine → mtime only, whatever runs locally under that PID.

**M6-4 — the app reads the cluster, not the machine.** This is the point of issue 7, and it is
visible for free in the compose setup: `search` and `detect` do not mount `$EPI_HOME`, so their PID
files never reach the app's machine.

```bash
ls $EPI_HOME/*.PID           # fs.PID only
```

- [ ] the app nonetheless shows the search and alerts services as **Running**, with `Host` reading
      `search` / `detect` — and `fs` marked `fs (this machine)` only when the app runs on the fs
      machine.

**M6-5 — the empty state.**

```bash
make compose-down; rm -rf $EPI_HOME/runners
```

- [ ] the table shows, in words: *"No runner holds a lease: no episomer service is running on any
      machine sharing this database."* — not an empty grid.

**M6-6 — a remote holder blocks the activation.** With `search` running in a container (host
`search`) and the app on the host, click **activate** for the message collection service.

- [ ] a modal appears *before* anything is spawned, saying in substance:

      Runner 'search' is already running on host 'search' (holding its lease since <UTC>, last seen
      <N> ago). Only one machine sharing this episomer database may run it at a time, so it has not
      been started here. Stop it on that machine, or wait for its lease to expire (TTL: 5 min), and
      try again.

- [ ] no process is spawned: `$EPI_HOME/jobs/search.log` gets no new run, no `search.PID` appears.
- [ ] same for the **fs** activation button.
- [ ] **Known gap, expected:** the *alerts* activate button does not call `loop_run_issues()` at
      all yet, so it still spawns a doomed process when another machine holds `detect`. Left for
      issue 9 — confirm it behaves that way rather than assuming it is fixed.

**M6-7 — the two deliberate silences.** Both look like missing checks on a quick read:

- [ ] a **stale** lease held by another machine → **no** modal, the start goes through and the
      claim succeeds. Taking over is what the standby is for.
- [ ] a lease held by **our own host** → no modal either; the local PID check refuses with the
      better message, the one naming the process to kill.

**M6-8 — the table refreshes on its own.** Leave the page open and stop a service.

- [ ] the row disappears (clean stop) or turns `stale` (hard kill) within ~10 seconds, with no
      manual refresh.

---

## 7. Configuration (issue 3)

**M7-1 — the knobs are env-overridable and arrive as numbers.**

```bash
EPISOMER_OPTION_RUNNER_TTL_SEARCH=45 Rscript -e '
  library(episomer); setup_config()
  v <- episomer:::runner_lease_ttl("search.bluesky")
  cat(v, class(v), "\n")'
```

- [ ] `45 numeric` — a string here would break every `difftime()` comparison downstream.
- [ ] **If the override is ignored**, check `grep runner_ttl $EPI_HOME/properties.json`: the merge
      order is `defaults < environment < properties.json`, so a value already saved by the app wins
      over the variable. That is the pre-existing `EPISOMER_OPTION_*` limitation documented in
      issue 3 — expected, not a regression.

**M7-2 — a bad value fails at startup, not three hours into a loop.**

```bash
EPISOMER_OPTION_RUNNER_TTL_SEARCH=abc Rscript -e 'library(episomer); setup_config()'
```

- [ ] `Invalid value 'abc' for environment variable EPISOMER_OPTION_RUNNER_TTL_SEARCH: expected a
      number` — the variable is **named** in the message.
- [ ] `EPISOMER_OPTION_SM_ACTIVATED_BLUESKY=1` is accepted as `TRUE` (compose files write booleans
      that way).

**M7-3 — the five knobs are deliberately absent from the configuration page.**

- [ ] no `runner_ttl_*` / `runner_heartbeat_span` / `runner_claim_guard` field anywhere in the
      Shiny configuration form. They are deployment knobs, and the page is open to non-admin users:
      a TTL mistyped there would break exclusivity for the whole cluster.

**M7-4 — a short TTL makes the whole plan cheap to replay.** For a fast run of blocks 4 and 6:

```bash
EPISOMER_OPTION_RUNNER_TTL_SEARCH=60 EPISOMER_OPTION_RUNNER_TTL_DETECT=60 \
EPISOMER_OPTION_RUNNER_HEARTBEAT_SPAN=10 make run-search
```

- [ ] failover happens in ~60 s instead of 5 to 30 minutes. Do **not** leave these values in a real
      deployment: a TTL close to the heartbeat produces false expiries under load (§9.2).

---

## 8. The fs API path, explicitly (issues 2 and 9)

The unit tests only exercise `fs_remote = NULL`. Everything above already went through HTTP in the
compose setup, but these two are worth isolating because they are the operations with no local
equivalent.

**M8-1 — `epifile.move()` over HTTP.** The release of M3-1 done from a container:

```bash
docker compose -f docker/compose.yml stop detect
ls $EPI_HOME/runners/old/
```

- [ ] the destination is resolved **relative to the fs machine's `epiHome`**
      (`runners/old/detect.<id>.json`), not as an absolute path or a URL. A 4xx in the fs log here
      means `to_remote_dest()` is sending the wrong shape.

**M8-2 — `epifile.mtime()` on a missing file.** The bug fixed on the way (`p` vs `path`, and the
bare logical `NA`) is only visible on a vector:

```r
episomer:::epifile.mtime(c(episomer:::runner_lease_path("detect"),
                           episomer:::runner_lease_path("does-not-exist")))
```

- [ ] a `POSIXct` vector of length 2 with an `NA` in second position — **not** a numeric vector.

---

## 9. Result sheet

| Block | Scope | Result | Notes |
|---|---|---|---|
| M0 | unit suite + toolkit | | |
| M1 | identity | | |
| M2 | lease file + heartbeat | | |
| M3 | release on exit | | |
| M4 | exclusivity | | |
| M5 | guards | | |
| M6 | Shiny | | |
| M7 | configuration | | |
| M8 | fs API path | | |

Known and accepted before you start, so they do not get reported as bugs (§9):

* the lease is **cooperative**, not a distributed lock: two runners can overlap for a few seconds
  around a claim race or a partition;
* failover latency **is** the TTL, by design;
* the parent `search` lease goes stale on a healthy machine (§9.5, still open — see the last
  note of issue 14);
* a service started while its runner is held by a live instance **waits** instead of failing (issue
  14): a container sitting on "waiting for it to be released or to expire" is working as intended;
* the alerts *activate* button is not gated yet (issue 9);
* `EPISOMER_OPTION_*` stops taking effect once the app has saved `properties.json`.
