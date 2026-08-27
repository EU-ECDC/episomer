# Multi-instance services — design & implementation plan

Answer to the questions raised in `inst/schema/multi_services.md`.
Nothing is implemented yet: this document is meant to be read, discussed, and then turned into
GitHub issues (§10). All the proposed code is **plain R** and uses only what the fs API already
exposes today (no Scala change required — see §3.1 for the price we pay for that).

---

## 1. TL;DR of the recommended design

Two independent layers, doing two different jobs:

| Layer | Where it lives | What it answers | Existing code |
|---|---|---|---|
| **PID file** (keep as is) | local disk of the machine running the service (`conf$data_dir/search.PID`) | "is there a process *on this machine* I can kill?" | `register_runner()`, `get_running_task_pid()`, `kill_task()` |
| **Lease file** (new) | on the fs machine, through the fs API (`<data_dir>/runners/search.bluesky.json`) | "who, in the whole cluster, is *allowed* to be the search runner right now?" | new `R/runners-lease.R` |

The lease is a **time-based lease** (a.k.a. lock with a TTL), exactly what you described:

* a runner **claims** the lease by writing a small JSON file holding its identity;
* it **renews** it regularly (`i_am_alive()`) — renewing is just touching the file so the fs machine
  bumps its mtime;
* anybody can decide whether a runner is alive by reading `mtime` and comparing it to a TTL;
* a runner **checks it still holds the lease before every meaningful write** and kills itself if it
  lost it.

The single most important design decision to make on top of your proposal: **the heartbeat must not
be tied to the API request loop** (§3.3), otherwise rate-limit waits will look like a dead service.

---

## 2. Vocabulary (worth fixing early, it will end up in function names)

* **service**: `fs`, `search`, `detect` — the kind of work.
* **runner name**: the unit of exclusivity, same granularity as today's PID files: `fs`, `detect`,
  `search`, `search.bluesky`, `search.mastodon.piaille.fr`, … Today `search_loop()` registers
  `search` and each worker registers `search.<network>` (`R/search.R:47` and `R/search.R:103`).
  Keep that granularity: it is already the right one, and it means two machines can legitimately
  split the collection (machine B does bluesky, machine C does mastodon).
* **instance**: one R process. Identified by a random `instance_id` generated at startup —
  your `conf$token_value`.
* **host**: informational, for the Shiny UI ("search.bluesky is running on machine-B").
* **lease**: the right, granted for `ttl` seconds and renewable, to act as a given runner name.

I would drop the word *token*: it suggests a secret / an authorisation credential. It is an
**identity**, not a secret (anybody who can reach the fs API can read it — see §9.4).

---

## 3. Discussion of your proposal

Overall: the proposal is sound and it is the standard way to do this. Five things need adjusting.

### 3.1 There is no atomic "write if unchanged" in the fs API — the claim needs a confirmation read

`POST /file/...?action=upload` (`scala/src/main/scala/API.scala:600`) always writes with
`REPLACE_EXISTING`: the write itself is atomic (`.part` file + `ATOMIC_MOVE`, so no reader ever sees
half a file), but it is **last-writer-wins**. There is no compare-and-swap, no
`If-Match`/ETag, no `createNew` flag on upload. So this sequence is possible:

```
B: read lease  -> absent          C: read lease  -> absent
B: write lease -> holder = B      C: write lease -> holder = C
B: "I own it"                     C: "I own it"        <-- both collecting, duplicate posts
```

Fix without touching the Scala side — **claim, wait, re-read**:

```
claim = write my identity
        wait guard_seconds (a few seconds, > worst-case write latency)
        re-read the file
        I hold the lease  <=>  the file still names me
```

If both wrote, the second write wins and the loser sees a foreign identity and backs off. The
window shrinks to "two writes landing within `guard_seconds` of each other with the *later* one
being read before it landed", which in practice does not happen for processes that start seconds
apart and is fully covered by the second safety net (`stop_if_lease_lost()` before every write).

Be honest about it in the code comment: this is a *cooperative* lease, not a distributed lock. If
we ever want a real one, the cheapest path is one new Scala action
(`?action=uploadIfUnchanged&md5=<previous md5>` returning 409) — worth a separate issue, but the R
side below does not need it.

### 3.2 Use the fs machine's clock, never the runners' clocks

Your instinct to compare "last modification date on machine A" is the right one and it is worth
saying why: `mtime` is produced by a **single** machine (the one running fs), so no NTP skew
between B and C can make a lease look fresh or expired when it is not. Consequence:

* freshness comes **only** from `epifile.mtime()` (`?action=mtime`),
* the timestamps we also store *inside* the JSON (`claimed_at`, `renewed_at`) are **for humans and
  the Shiny UI only** — never for the expiry decision.

Also: renewing does not need to rewrite the content. `epifile.touch()` maps to
`Files.setLastModifiedTime` (`API.scala:578`), so the regular heartbeat is one tiny POST, no body.
Only `claim` writes JSON.

Small existing bug to fix on the way, since the lease code will lean on it: `epifile.mtime()`
(`R/fs.R:150`) tests `epifile.exists(path)` instead of `epifile.exists(p)` inside its own `lapply`
— harmless for single paths, wrong for vectors.

### 3.3 The heartbeat must be independent from the API request rhythm

You asked: *"for the search_loop we could send the message that we are still alive at each API query
-> what about when we reach the max API rate and we must wait for x minutes?"* — that is precisely
why beating on each API query is not enough. The search loop already sleeps for arbitrarily long:
`Sys.sleep(wait_for)` at `R/search.R:177`, where `wait_for` comes from `can_wait_for()` and is
bounded only by `collect_span` (60 min by default).

Two candidate solutions:

* **(A) chunked sleep — recommended.** Replace every long `Sys.sleep()` in the loops with
  `sleep_with_heartbeat(seconds)`, which sleeps in `heartbeat_span`-sized slices and renews the
  lease between slices. Three call sites only: `R/search.R:177`, `R/scheduler.R:293`,
  `R/alert.R:601`. Pure R, no extra process, and the heartbeat keeps proving that *the loop itself*
  is alive.
* **(B) background heartbeat process** (`future`, as `fs_loop()` already does for `fs_mon`,
  `R/fs.R:247`). More machinery, and worse semantics: a background beater keeps shouting "alive"
  while the main loop is deadlocked, which is the exact failure we want to detect.

Go with (A). Use (B) only for `detect`, and only if §3.5 turns out to be a real problem.

### 3.4 Releasing the lease on exit — and the missing `delete` action

You did not mention release, and it matters for the operational story: without it, restarting a
service means waiting for the full TTL before it can collect again (or, worse, tempts us into a very
short TTL). Graceful release should be an `on.exit()` in each loop.

Annoyance: the fs API has **no delete action** (post actions are `upload`, `createDir`, `touch`,
`copy` — `API.scala:566`). So "release" cannot remove the file. Workaround, no Scala change needed:
write the lease with `holder = null, released = true`. `read_runner_lease()` treats a released file
as free regardless of its mtime. Costs one field and one condition.

### 3.5 Long tasks in the detect loop deserve a longer TTL

`detect` executes tasks that can run for a very long time (`generate_alerts()`, geolocation and
aggregation waits). Between two `save_tasks()` there can be a long silence, and (A) only covers the
*sleeping* parts, not a long-running task. Therefore: **per-service TTL**, not one global value:

| runner | heartbeat | TTL | rationale |
|---|---|---|---|
| `fs` | 30 s | 120 s | fs is also monitored by `/ping`, cheap to detect |
| `search.*` | 30 s | 300 s | your 5-minute proposal; longest silence is a chunked sleep |
| `detect` | 30 s | 1800 s | one long task must not look dead |

A long TTL is not a correctness problem (the second instance simply waits longer before taking
over), it only slows down failover. Correctness comes from `stop_if_lease_lost()`.

### 3.6 Where your "it's me, I can start again" case lands

Your three cases become simpler once freshness is decided by mtime alone:

* *"updated 30 s ago by me"* → the file names my `instance_id` → renew, keep working.
* *"updated 10 min ago by me"* → **expired is expired**: the lease is free, so I claim it like
  anybody else. No special case needed. (A new process has a new `instance_id` anyway, so
  "recognising myself" across a restart cannot work at instance level.)
* *"updated by someone else, recently"* → I am not the holder → back off, or kill myself if I
  believed I held it.

The one case worth an optimisation later: **crash + fast restart on the same host**. There, `host`
matches and the `pid` recorded in the lease is a *local* PID we can actually test with the existing
`get_running_task_pid()` machinery — so we can reclaim immediately instead of waiting for the TTL.
Nice-to-have, issue #7.

---

## 4. The lease file

One file per runner name, under a dedicated folder so listing them is trivial for the Shiny app:

```
<data_dir>/runners/fs.json
<data_dir>/runners/detect.json
<data_dir>/runners/search.json
<data_dir>/runners/search.bluesky.json
<data_dir>/runners/search.mastodon.piaille.fr.json
```

Accessed through `epipath("runners", paste0(name, ".json"))`, which means **the exact same code
works for the local fs service (`fs_remote = NULL`, plain file access) and for remote services
(HTTP)** — that is the main reason to build on `epipath()` / `write_json_atomic()` /
`epifile.mtime()` / `epifile.touch()` rather than on new HTTP calls.

Content:

```json
{
  "runner": "search.bluesky",
  "service": "search",
  "instance_id": "8f3c1a9e6b4d2f70",
  "host": "machine-B",
  "pid": 12345,
  "episomer_version": "4.0.5",
  "claimed_at": "2026-08-27T09:14:03Z",
  "renewed_at": "2026-08-27T09:41:33Z",
  "released": false
}
```

* `instance_id`: `paste(format(as.hexmode(sample.int(2^31, 4)), width = 8), collapse = "")` or
  simply `basename(tempfile(""))` — uniqueness within the cluster is what matters, not
  cryptographic quality.
* `host`: `Sys.getenv("EPISOMER_INSTANCE", Sys.info()[["nodename"]])`. Container hostnames are
  random hex, so let deployments override it — worth adding to `docker/compose.yml`.
* `claimed_at` / `renewed_at` / `pid` / `episomer_version`: diagnostics and Shiny display only
  (§3.2). `renewed_at` is only refreshed when the content is rewritten, so treat it as "since when
  is this instance the holder", and display mtime for "last seen".

---

## 5. Proposed R API (`R/runners-lease.R`)

Mapping with the names you proposed:

| Your name | Proposed name | Why |
|---|---|---|
| `i_am_alive` | `renew_runner_lease(name)` | says what it does to what object |
| `is_it_me_currently_working` | `i_hold_runner_lease(name)` | reads as a predicate |
| `should_i_kill_myself` | `stop_if_lease_lost(name)` | matches the existing `stop_if_no_config()` / `stop_if_no_fs()` idiom, and makes the call site self-documenting |
| — | `claim_runner_lease(name)` | acquisition, needs to exist separately from renewal (§3.1) |
| — | `release_runner_lease(name)` | §3.4 |
| — | `read_runner_lease(name)` / `runner_lease_status(name)` | read-only, used by the Shiny app and `is_*_running()` |

Skeletons for the two subtle ones (the rest is straightforward and better written by hand):

```r
# TRUE if this process is now the legitimate holder of `name`.
# Cooperative lease: see the comment about the missing compare-and-swap in the fs API.
claim_runner_lease <- function(name, guard = 5) {
  stop_if_no_config()
  current <- read_runner_lease(name)
  if (!is.null(current) && !lease_is_free(current, name)) {
    return(FALSE)                    # someone else is alive and holding it
  }
  write_runner_lease(name)           # optimistic write
  Sys.sleep(guard)                   # let a competing write land
  confirmed <- read_runner_lease(name)
  !is.null(confirmed) && identical(confirmed$instance_id, conf$instance_id)
}

# A lease is free if absent, explicitly released, expired, or already mine.
lease_is_free <- function(lease, name) {
  if (is.null(lease) || isTRUE(lease$released)) return(TRUE)
  if (identical(lease$instance_id, conf$instance_id)) return(TRUE)
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  age > runner_lease_ttl(name)       # seen_at comes from epifile.mtime(), never from the JSON
}

# Sleep, renewing the lease along the way, and abort early if it was stolen.
sleep_with_heartbeat <- function(seconds, name = conf$runner_name) {
  deadline <- Sys.time() + seconds
  repeat {
    left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (left <= 0) break
    Sys.sleep(min(left, conf$runner_heartbeat_span))
    renew_runner_lease(name)         # epifile.touch(), cheap
    stop_if_lease_lost(name)
  }
}
```

`stop_if_lease_lost()` should signal a **typed condition**, not a bare `stop()`, so the loops can
tell "I lost my spot" (expected, exit quietly with a clear log line) from a real error:

```r
stop_if_lease_lost <- function(name = conf$runner_name) {
  if (!i_hold_runner_lease(name)) {
    rlang::abort(
      sprintf("Runner lease '%s' was taken by another instance, stopping", name),
      class = "episomer_lease_lost"
    )
  }
}
```

`rlang` is already a dependency (used in `R/fs.R`, `R/search.R`).

### How a runner dies

`kill_task()` stays as is — it is the *local* tool. A runner that lost its lease should not need to
be killed from outside: it raises `episomer_lease_lost`, the top of the loop catches that class,
logs, releases nothing (the lease is not ours anymore — **do not** write a release, that would
erase the new holder), and exits. For `search_loop()`, workers run inside a PSOCK cluster
(`R/search.R:57`): a worker raising the condition must be allowed to end without taking the whole
cluster down in an unexplained way — that is where the current `stop("All search loop proccesses
ended, which is unexpected")` at `R/search.R:74` needs a nuance.

---

## 6. Wiring into the existing code

### 6.1 `register_*_runner()` — keep the PID file, add the lease

`register_runner(name)` (`R/runners.R:54`) currently: local PID check + write PID file. New
behaviour, in order:

1. local PID check + write PID file — unchanged (protects against two instances *on the same
   machine*, and gives `kill_task()` something to work with);
2. `claim_runner_lease(name)`; if it returns `FALSE`, stop with an explicit message naming the
   current holder's `host` — this is the "preventing multiple instances of the same runner" of the
   objective;
3. store `conf$runner_name` so the heartbeat helpers do not need the name passed around;
4. `on.exit(release_runner_lease(name))` in the caller (the loop), not in `register_runner()`.

### 6.2 `is_*_running()` — read the lease instead of the local PID

* `is_search_running()` (`R/runners.R:472`) and `is_detect_running()` (`R/runners.R:528`) become
  "the lease exists and is fresh" instead of `get_running_task_pid(...) >= 0`. This is what makes
  the Shiny app work in a distributed setup, which is the whole point.
* `missing_search_jobs()` (`R/runners.R:447`) same change, per active social media.
* `is_fs_running()` (`R/runners.R:492`) keeps its `/ping` implementation: it is better than a
  lease (instantaneous, no TTL lag). `fs` still takes a lease, but for a different reason: to stop a
  second fs from opening the same Lucene storage.
* **Refinement worth having:** in a single-machine setup the lease introduces a regression in
  responsiveness — after a crash, the Shiny app shows "running" for up to TTL. Cheap fix: if the
  lease's `host` is my host, cross-check the recorded `pid` locally. Both layers already exist.
* Callers to re-read once the semantics change: `R/check.R:348,361,374,387`,
  `R/shiny-app-utils.R:141-144`, `R/setup.R:638-656,768`, `R/search.R:120`.

### 6.3 Guard points — where "am I still allowed to write?"

Your two candidates are the right ones, plus one you did not list (which is the one that actually
corrupts data):

- [ ] `search_loop_worker()` before saving plans: `R/search.R:171-176` (`commit_posts()` +
      `save_config(sm_topics = ...)`) and the periodic save at `R/search.R:265-267`.
- [ ] `save_tasks()` in the detect loop: `R/scheduler.R:150`, called at lines 250, 262, 359.
      Guarding inside `save_tasks()` itself is tempting but it is also called from the Shiny app
      (which is not a lease holder) — so guard at the **call sites in `detect_loop()`**, not inside.
- [ ] **`search_topic()` before POSTing posts to the fs — `R/search.R:320`.** Two search loops
      running the same topic do not just fight over `topics.json`, they push the same posts twice.
      This is the guard that protects the data; the `topics.json` one only protects the bookkeeping.
- [ ] `register_series()` and the geolocation/aggregation triggers fired from `detect_loop()`.

Guarding costs one HTTP GET (mtime) + one GET (content). At these frequencies it is free; do not
guard inside tight loops.

### 6.4 The Shiny app is *not* an exclusive runner

The brief says "Shiny app will follow the same workflow as services" — I would qualify that:
several people must be able to open the app at once, so the app must never take an exclusive lease.
Give it a **presence** entry instead (same file format, in `runners/app.<instance_id>.json`, no
exclusivity, no `stop()`), useful to display "3 sessions connected, on 2 hosts". The app does write
`properties.json` and the xlsx files, so if we ever want to serialise those writes it should be a
separate, much shorter-lived lock (out of scope here — §9.3).

### 6.5 New configuration properties

Added to `get_empty_config()` (`R/config.R:~222`, next to `fs_url`/`fs_port`), read in
`setup_config()` (`R/config.R:390`), written in `save_config()` (`R/config.R:655`):

| Property | Default | Notes |
|---|---|---|
| `runner_heartbeat_span` | 30 | seconds between renewals |
| `runner_ttl_search` | 300 | |
| `runner_ttl_detect` | 1800 | |
| `runner_ttl_fs` | 120 | |
| `runner_claim_guard` | 5 | the `guard` of §3.1 |

They become overridable as `EPISOMER_OPTION_RUNNER_TTL_SEARCH` etc. for free via `get_env_props()`
(`R/config.R:1110`). **Careful**: env props arrive as *strings*; these are used in arithmetic, so
coerce with `as.numeric()` when reading them (same latent trap as `fs_port`).

`conf$instance_id`, `conf$instance_host` and `conf$runner_name` are **session state, not
configuration**: set them in `setup_config()` but never write them to `properties.json` (they must
not be shared between machines).

---

## 7. Walk-through of your machine A / B / C scenario

| # | Situation | What happens |
|---|---|---|
| 1 | B starts `search.bluesky`, lease absent | B writes, waits 5 s, re-reads, sees itself → collects |
| 2 | C starts `search.bluesky` 1 min later | C reads the lease: fresh, foreign → `register_search_runner()` stops with "already held by machine-B since …". C's container restarts and retries — this is the intended standby behaviour |
| 3 | B and C start simultaneously | both write; the later write wins; the loser's confirmation read shows a foreign id → it backs off (§3.1) |
| 4 | B is stopped cleanly | `on.exit` writes `released = true` → C claims within one restart cycle, no TTL wait |
| 5 | B's host dies | no renewal; after `runner_ttl_search` the lease is stale → C claims it |
| 6 | B is frozen (network partition), C takes over, then B wakes up | B's next `stop_if_lease_lost()` — before its next post upload — raises `episomer_lease_lost`; B exits without writing. Worst case: B pushed posts during the partition window before its guard; deduplication on the fs side limits the damage, but this is the accepted residual risk (§9.1) |
| 7 | B does bluesky, C does mastodon | different runner names, different leases, both work. This is the useful multi-machine setup and it works with no extra code |
| 8 | Shiny app on machine D | reads `runners/*.json` through the fs API, displays service, host, last seen. No lease |

---

## 8. What the Shiny app should now display

Worth doing at the same time, because it is what makes the whole thing debuggable: the services
status block (`R/shiny-app.R:2224-2252`, fed by `R/shiny-app-utils.R:141-144`) should show, per
runner: name, status (running / stale / released / free), **host**, and "last seen N s ago". One
`epilist.files(epipath("runners"))` + one read per file. Without this, a lost lease is invisible
and the operator has to read logs on three machines.

---

## 9. Known limits, accepted explicitly

1. **Cooperative, not bulletproof.** Two runners can overlap for a few seconds around a claim race
   or a network partition (§3.1, case 6). Mitigated by the guard before every write, not eliminated.
   A real fix needs one conditional-write endpoint on the Scala side.
2. **Failover latency = TTL.** By design. Do not lower the TTL below a few multiples of the
   heartbeat to "improve" it: that just makes false expiries under load.
3. **Concurrent writers to `properties.json` / xlsx** (several Shiny apps, or an app and a service)
   are *not* addressed here and were not addressed before either. Separate concern.
4. **No authentication.** Anybody who can reach the fs API can read, steal, or release any lease.
   That is already true of every other file served by the fs API; it belongs to a network/deployment
   issue, not to this one.
5. `search`'s parent lease (`search`, taken by `search_loop()` on top of the per-network ones)
   means one machine cannot run only *some* networks while another runs the rest **unless** the
   parent lease is dropped or made non-exclusive. Decide this explicitly — I would make the parent
   entry presence-only and keep exclusivity at `search.<network>` level, which is where the
   duplication risk actually is.

---

## 10. Proposed GitHub issues

Sized so each is a reviewable PR, and ordered so each one is testable on its own.

### Issue 1 — Instance identity in `conf`
- [ ] `conf$instance_id` generated once per process in `setup_config()`
- [ ] `conf$instance_host` from `EPISOMER_INSTANCE`, falling back to `Sys.info()[["nodename"]]`
- [ ] never persisted to `properties.json` (check both `setup_config()` and `save_config()`)
- [ ] `EPISOMER_INSTANCE` documented and set in `docker/compose.yml` for the three services
- [ ] unit test: two `setup_config()` calls in the same session keep the same `instance_id`

### Issue 2 — Lease primitives (`R/runners-lease.R`)
- [ ] `runner_lease_path(name)` on top of `epipath("runners", ...)`, folder created if missing
- [ ] `read_runner_lease(name)` → `NULL` or a list including `seen_at` from `epifile.mtime()`
- [ ] `runner_lease_ttl(name)` → per-service TTL from `conf`
- [ ] `lease_is_free(lease, name)`
- [ ] `write_runner_lease(name)` via `write_json_atomic()`
- [ ] `claim_runner_lease(name, guard)` with the confirmation read of §3.1
- [ ] `renew_runner_lease(name)` via `epifile.touch()`
- [ ] `release_runner_lease(name)` writing `released = true` (§3.4)
- [ ] `i_hold_runner_lease(name)`, `runner_lease_status(name)`
- [ ] `stop_if_lease_lost(name)` raising the `episomer_lease_lost` condition
- [ ] fix the `p`/`path` mix-up in `epifile.mtime()` (`R/fs.R:150`)
- [ ] tests (see §11) — this issue is where the test effort belongs

### Issue 3 — New configuration properties
- [ ] `runner_heartbeat_span`, `runner_ttl_search`, `runner_ttl_detect`, `runner_ttl_fs`,
      `runner_claim_guard` in `get_empty_config()` / `setup_config()` / `save_config()`
- [ ] numeric coercion of the values coming from `get_env_props()`
- [ ] exposed on the Shiny configuration page (or deliberately left env-only — decide)

### Issue 4 — `register_runner()` claims a lease
- [ ] `register_runner()` = PID file (unchanged) + `claim_runner_lease()`
- [ ] failure message naming the current holder (`host`, since when)
- [ ] `conf$runner_name` set
- [ ] `release_runner_lease()` on exit in `fs_loop()`, `search_loop_worker()`, `detect_loop()`
- [ ] `stop_all_runner_tasks()` still works locally (it must not depend on the lease)

### Issue 5 — Heartbeat during long waits
- [ ] `sleep_with_heartbeat(seconds, name)`
- [ ] replaces `Sys.sleep()` at `R/search.R:177`, `R/scheduler.R:293`, `R/alert.R:601`
- [ ] `renew_runner_lease()` also called once per iteration at the top of each loop
- [ ] check that no code path in `search_loop_worker()` can stay silent longer than the TTL
      (rate-limit waits are the case to verify, §3.3)

### Issue 6 — Guard the writes
- [ ] `stop_if_lease_lost()` before `commit_posts()` / `save_config()` in `search_loop_worker()`
      (`R/search.R:171`, `R/search.R:265`)
- [ ] before POSTing posts in `search_topic()` (`R/search.R:320`)
- [ ] before each `save_tasks()` **call site** in `detect_loop()` (not inside `save_tasks()`)
- [ ] before `register_series()` and the geolocation/aggregation triggers
- [ ] `episomer_lease_lost` caught at the top of each loop: log, exit cleanly, do not release
- [ ] `search_loop()` cluster: one worker exiting on a lost lease must produce a clear log line,
      not the current unexplained `stop()` at `R/search.R:74`

### Issue 7 — `is_*_running()` reads the lease
- [ ] `is_search_running()`, `is_detect_running()`, `missing_search_jobs()` based on the lease
- [ ] `is_fs_running()` keeps `/ping`
- [ ] local cross-check on the PID when `lease$host == conf$instance_host` (§6.2), which also gives
      the fast reclaim of §3.6
- [ ] review the call sites listed in §6.2 (`check.R`, `setup.R`, `shiny-app-utils.R`)

### Issue 8 — Shiny app: distributed services status
- [ ] `runner_leases()` listing everything under `runners/`
- [ ] status table: runner, status, host, last seen, since
- [ ] presence entry for the app itself (`runners/app.<instance_id>.json`, no exclusivity, §6.4)
- [ ] `loop_run_issues()` (`R/runners.R:572`) messages updated for the remote case ("running on
      another host" is not the same thing as "not running")

### Issue 9 — Documentation & deployment
- [ ] vignette / README section: how to run one fs + N machines
- [ ] `docker/compose.yml`: a second `search` service to demo standby, `EPISOMER_INSTANCE` set
- [ ] update `inst/schema/services.excalidraw` with the lease layer
- [ ] operator guide: how to read `runners/*.json` and what "stale" means

### Issue 10 (optional, Scala) — Conditional write in the fs API
- [ ] `POST /file/...?action=upload&ifMd5=<md5>` returning 409 on mismatch
- [ ] `?action=delete` (removes the `released` marker workaround of §3.4)
- [ ] `claim_runner_lease()` uses it when available, keeps the current path as a fallback

**Recommended order:** 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9. Issues 1–4 give a working exclusion
mechanism; 5–6 make it safe; 7–8 make it visible.

---

## 11. Tests to add

The design is deliberately built so that almost everything is testable **without any network and
without a second machine**: with `fs_remote = NULL`, `epipath()` resolves to a temp dir, and a
"second instance" is just a different `conf$instance_id`. Follow the conventions of
`TESTING_ROADMAP.md` (`local_conf()` from `tests/testthat/helper-conf.R`, testthat 3e).

Suggested helper:

```r
# runs `code` as if it were another episomer instance
with_instance <- function(id, host = "other-host", code) { ... }
```

Cases worth covering, in `tests/testthat/test-runners_lease.R`:

- [ ] free lease (absent file) → claim succeeds
- [ ] fresh foreign lease → claim fails, the holder is reported
- [ ] expired foreign lease → claim succeeds (mock `epifile.mtime()` to fake age, or
      `Sys.setFileTime()` in local mode — the latter is a real test of the mtime path)
- [ ] released foreign lease → claim succeeds regardless of mtime
- [ ] my own lease → `i_hold_runner_lease()` TRUE, `stop_if_lease_lost()` silent
- [ ] stolen lease → `stop_if_lease_lost()` raises a condition of class `episomer_lease_lost`
- [ ] `renew_runner_lease()` moves the mtime forward and does **not** change the content
- [ ] per-service TTL: `runner_lease_ttl("search.bluesky")` → `conf$runner_ttl_search`
- [ ] `sleep_with_heartbeat(2)` with `runner_heartbeat_span = 1` renews at least once (mock
      `renew_runner_lease()` and count calls — do not sleep for real in the suite)
- [ ] `is_search_running()` on a stale lease → FALSE; on a fresh one → TRUE

Not unit-testable, keep for a manual/integration check on `docker compose`: the simultaneous-claim
race (§3.1, case 3) and the takeover after killing a container (case 5).
