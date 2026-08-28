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

### 3.4 Releasing the lease on exit — use the `move` action

You did not mention release, and it matters for the operational story: without it, restarting a
service means waiting for the full TTL before it can collect again (or, worse, tempts us into a very
short TTL). Graceful release should be an `on.exit()` in each loop.

The fs API still has **no delete action**, but since `c499fbe` it has a `move` one (post actions are
`upload`, `createDir`, `touch`, `move` — `API.scala:566`; note that `move` *replaced* `copy`, it was
not added next to it — see the note at the end of this section). `Files.move` with
`REPLACE_EXISTING` is enough for a clean release: **move the lease out of the way** instead of
marking it.

```
release = move  <data_dir>/runners/search.bluesky.json
             -> <data_dir>/runners/old/search.bluesky.<instance_id>.json
```

Why this is better than the `released = true` marker first considered here:

* `read_runner_lease()` has one less case to handle — the file is simply absent, which is already
  the "free" case;
* `epilist.files(epipath("runners"))` then lists **live runners only**, which is exactly what the
  Shiny status table of §8 wants to show;
* the released files are kept, not lost: `runners/old/` is a trail an operator can read after the
  fact ("who held this and until when"). Careful with the mtime there: moving a file preserves it,
  both locally (`file.rename()`) and remotely (`Files.move`), so what it dates is the **last
  heartbeat** of the holder, not the release. That is the useful value anyway — it is the last
  moment that runner was known alive. The `<instance_id>` in the name keeps successive releases
  from overwriting each other, and nothing lists that folder, so the accumulation costs nothing but
  disk.

Same precaution as with the marker, and it is the important one: **release only if we still hold
the lease**. A move issued by an instance that silently lost its lease would take away the *new*
holder's file. `release_runner_lease()` must therefore check `i_hold_runner_lease()` immediately
before moving, and the `episomer_lease_lost` handler of §5 must not release at all.

Two consequences on the R side, both belonging to issue 2:

* `epifile.move(from, to)` has to be written in `R/fs.R` (local branch: `file.rename()`; remote
  branch: `POST ?action=move` with the destination as JSON body, like the former `epifile.copy()`);
* **`epifile.copy()` (`R/fs.R:126`) is now broken for the remote→remote case**: it still posts
  `?action=copy`, an action the server no longer knows. No current call site hits that branch (the
  Shiny download handlers copy to a local destination, `alert.R:2158` and `geo-tagging.R:898` upload
  a local temp file), so this is latent rather than live. It cannot be fixed by simply swapping the
  action name — `move` does not copy — so the call is either to restore `copy` in the Scala API next
  to `move`, or to drop that branch of `epifile.copy()`. To be decided with whoever wrote `c499fbe`;
  the lease code itself only needs `move`.

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
<data_dir>/runners/old/search.bluesky.<instance_id>.json   <- released, kept as a trail (§3.4)
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
  "renewed_at": "2026-08-27T09:41:33Z"
}
```

There is no `released` field: a released lease is a file that has been *moved* to `runners/old/`
(§3.4), so the presence of the file under `runners/` already means "somebody claims this runner".

* `instance_id`: **implemented** (issue 1) as `conf$instance_id`, built by `new_instance_id()`
  (`R/config.R`) from `tempdir()` / `tempfile()`. Uniqueness within the cluster is what matters,
  not cryptographic quality — but note the reason `sample.int()` was *not* used: it consumes the R
  random number generator, so a script calling `set.seed()` before `setup_config()` would get a
  predictable, and therefore possibly colliding, identity.
* `host`: **implemented** (issue 1) as `conf$instance_host`, built by `get_instance_host()` from
  `EPISOMER_INSTANCE` with a fallback on `Sys.info()[["nodename"]]`, and set for the three services
  in `docker/compose.yml`. Container hostnames are random hex, hence the override.
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

# A lease is free if absent (never claimed, or released and moved to runners/old/ — §3.4),
# expired, or already mine.
lease_is_free <- function(lease, name) {
  if (is.null(lease)) return(TRUE)
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
logs, releases nothing (the lease is not ours anymore — **do not** release, the move would take
away the new holder's file), and exits. For `search_loop()`, workers run inside a PSOCK cluster
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

- [x] `search_loop_worker()` before saving plans (`commit_posts()` +
      `save_config(sm_topics = ...)`) and before the periodic `commit_posts()`.
- [x] `save_tasks()` in the detect loop.
      Guarding inside `save_tasks()` itself is tempting but it is also called from the Shiny app
      (which is not a lease holder) — so guard at the **call sites in `detect_loop()`**, not inside.
      Done at three of them: before `plan_tasks()` (which saves as soon as it reschedules), before
      the save of an aborted task, and before the save that follows a task run — plus
      `generate_alerts()` itself, which writes much more than `tasks.json`.
- [x] **`search_topic()` before POSTing posts to the fs.** Two search loops
      running the same topic do not just fight over `topics.json`, they push the same posts twice.
      This is the guard that protects the data; the `topics.json` one only protects the bookkeeping.
      Placed *after* the social media request: the request writes nothing, everything after it does.
- [x] `register_series()` (the aggregation trigger). The geolocation one turned out not to be a
      call of its own — it is the `geolocate=true` of the post upload, guarded in `search_topic()`.

Guarding costs one HTTP GET (mtime) + one GET (content). At these frequencies it is free; do not
guard inside tight loops.

### 6.4 The Shiny app is *not* a runner at all — decided

The brief says "Shiny app will follow the same workflow as services". **It does not**, and this is
settled: the app writes nothing under `runners/`, neither a lease nor a presence entry. It is a
pure reader of the runner state.

That is already how the code is built, and it is worth stating so nobody "completes" it later:
`register_runner()` is called by `fs_loop()`, `search_loop()` / its workers, `detect_loop()` and
`fs_mon` only (`R/fs.R:275`, `R/runners.R:2-49`, `R/scheduler.R:197`, `R/search.R:47`). The app
merely *reads* the state (`is_*_running()`, §6.2) and *starts* services
(`register_*_runner_task()`, `R/runners.R:270`), which spawn separate processes — those processes
take the lease, not the app.

Two reasons not to give it even a presence entry:

* several people must be able to open the app at once, so anything exclusive is wrong by
  construction, and a non-exclusive entry buys only the cosmetic "3 sessions connected on 2 hosts";
* one file per session would accumulate under `runners/` with no natural moment to clean it up (a
  browser tab closing is not an event the R process reliably sees), and it would pollute the very
  listing the status table of §8 reads.

The app does write `properties.json` and the xlsx files, so if we ever want to serialise *those*
writes it should be a separate, much shorter-lived lock (out of scope here — §9.3).

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
| 4 | B is stopped cleanly | `on.exit` moves the lease to `runners/old/` → C sees a free runner and claims within one restart cycle, no TTL wait |
| 5 | B's host dies | no renewal; after `runner_ttl_search` the lease is stale → C claims it |
| 6 | B is frozen (network partition), C takes over, then B wakes up | B's next `stop_if_lease_lost()` — before its next post upload — raises `episomer_lease_lost`; B exits without writing. Worst case: B pushed posts during the partition window before its guard; deduplication on the fs side limits the damage, but this is the accepted residual risk (§9.1) |
| 7 | B does bluesky, C does mastodon | different runner names, different leases, both work. This is the useful multi-machine setup and it works with no extra code |
| 8 | Shiny app on machine D | reads `runners/*.json` through the fs API, displays service, host, last seen. Writes nothing there — it is not a runner (§6.4) |

---

## 8. What the Shiny app should now display

Worth doing at the same time, because it is what makes the whole thing debuggable: the services
status block (`R/shiny-app.R:2224-2252`, fed by `R/shiny-app-utils.R:141-144`) should show, per
runner: name, status (running / stale / free), **host**, and "last seen N s ago". One
`epilist.files(epipath("runners"))` + one read per file — `runners/old/` is deliberately not
listed, a released runner is simply one whose file is gone (§3.4). Without this, a lost lease is
invisible and the operator has to read logs on three machines.

The app itself does not appear in that table: it is not a runner (§6.4).

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
- [x] `conf$instance_id` generated once per process in `setup_config()`
- [x] `conf$instance_host` from `EPISOMER_INSTANCE`, falling back to `Sys.info()[["nodename"]]`
- [x] never persisted to `properties.json` (check both `setup_config()` and `save_config()`)
- [x] `EPISOMER_INSTANCE` documented and set in `docker/compose.yml` for the three services
- [x] unit test: two `setup_config()` calls in the same session keep the same `instance_id`

### Issue 2 — Lease primitives (`R/runners-lease.R`)
- [x] `runner_lease_path(name)` on top of `epipath("runners", ...)`, folder created if missing
- [x] `read_runner_lease(name)` → `NULL` or a list including `seen_at` from `epifile.mtime()`
- [x] `runner_lease_ttl(name)` → per-service TTL from `conf`
- [x] `lease_is_free(lease, name)`
- [x] `write_runner_lease(name)` via `write_json_atomic()`
- [x] `claim_runner_lease(name, guard)` with the confirmation read of §3.1
- [x] `renew_runner_lease(name)` via `epifile.touch()`
- [x] `epifile.move()` in `R/fs.R` (`?action=move` remotely, `file.rename()` locally), and fix or
      drop the now dead remote→remote branch of `epifile.copy()` (§3.4)
- [x] `release_runner_lease(name)` moving the file to `runners/old/<name>.<instance_id>.json`,
      **only if `i_hold_runner_lease()` is still true** (§3.4)
- [x] `i_hold_runner_lease(name)`, `runner_lease_status(name)`
- [x] `stop_if_lease_lost(name)` raising the `episomer_lease_lost` condition
- [x] fix the `p`/`path` mix-up in `epifile.mtime()` (`R/fs.R:150`)
- [x] tests (see §11) — this issue is where the test effort belongs

Diagram of the implemented logic: `inst/schema/runners_lease.excalidraw` (the file, taking the
lease with its confirmation read, the life of the holder, the per-service TTL).

Decided while implementing, all four visible in `R/runners-lease.R` / `R/fs.R`:

* **`epifile.copy()` remote→remote is kept, not dropped**, since `?action=copy` is being restored in
  the Scala API. Its body was wrong on top of the action name: the API resolves the destination
  against its own `epiHome` and rejects an absolute path, so both `move` and `copy` now send
  `to_remote_dest(to)` (`R/paths.R`, e.g. `runners/old/detect.abc.json`) instead of the full URL.
  **To confirm with the restored endpoint**: that it reads the destination as a relative
  `epiHome` path from a JSON string body, exactly like `move` does.
* **`runner_lease_path(name, create_dir = FALSE)`**: the `runners/` folder is created by the writers
  (`write_runner_lease()`, `release_runner_lease()`) rather than by every call. Reading a lease is
  far more frequent than writing one — every guard point does it — and a reader must not create
  anything on the fs machine, remotely at the price of an extra HTTP request.
* **TTL defaults live in `runner_lease_ttl_defaults()`** until issue 3 puts
  `runner_ttl_search` & co. in `conf`. `runner_lease_ttl()` already reads `conf` first, so issue 3
  is purely additive; same for `conf$runner_claim_guard` and `runner_claim_guard()`.
* **`epifile.mtime()` had a second bug in the same `lapply`**: the missing-file branch returned a
  bare (logical) `NA`, which turned the whole `c()`-ed result into numbers. It now returns
  `as.POSIXct(NA)`, which the lease code needs since it feeds those values to `difftime()`.

Not in this issue, on purpose: `sleep_with_heartbeat()` (issue 5) and everything wiring the
primitives into `register_runner()` and the loops (issues 4 and 6).

### Issue 3 — New configuration properties
- [x] `runner_heartbeat_span`, `runner_ttl_search`, `runner_ttl_detect`, `runner_ttl_fs`,
      `runner_claim_guard` in `get_empty_config()` / `setup_config()` / `save_config()`
- [x] numeric coercion of the values coming from `get_env_props()`
- [x] exposed on the Shiny configuration page (or deliberately left env-only — decide)
- [x] tests: `tests/testthat/test-config_runner_props.R`

Decided while implementing:

* **Not on the Shiny configuration page, deliberately.** These five are deployment knobs, not
  analysis settings, and the page is open to non-admin users: a TTL mistyped there breaks
  exclusivity for the *whole cluster* at once, not just for the machine running the app. The
  reason is written as a comment in `get_empty_config()` so this reads as a decision and not as an
  oversight. If operators end up needing visibility, the read-only display belongs next to the
  services status table of issue 8, not in the editable form.
* **The coercion is generic, not five hand-written `as.numeric()`.** `coerce_env_props(props,
  defaults)` (`R/config.R`, next to `get_env_props()`) gives each environment value the type of the
  matching default in `get_empty_config()`. One place holds the knowledge of types, and the same
  latent trap on `fs_port`, `smtp_port` and the alert thresholds is fixed on the way. Untypable
  values are an error naming the variable rather than a silent `NA`: an unusable
  `EPISOMER_OPTION_*` must fail at startup, not three hours into a loop. Defaults that are `NA`
  (`lang_updated_on` and the other date properties) are skipped on purpose — `NA` is a *logical* in
  R and coercing those would destroy them.
* **`1` / `0` are accepted for a boolean property**, since that is a natural way to write one in a
  compose file and `as.logical("1")` is `NA`.

**Found on the way, not fixed here — worth its own issue.** `EPISOMER_OPTION_*` is much weaker than
it looks, and this is pre-existing behaviour, unrelated to the lease work:

1. `setup_config()` only reads the environment **inside** the `if (epifile.exists(props_path) ||
   !is.null(fs_remote))` branch, so on a fresh local data directory with no `properties.json` the
   environment is not read at all;
2. the merge order is `defaults < environment < properties.json`, so a property already stored in
   the file wins over the variable.

Since `save_config()` writes every property, an `EPISOMER_OPTION_*` therefore stops having any
effect as soon as the app has saved the configuration once. `EPISOMER_OPTION_FS_URL` in
`docker/compose.yml` is already in that situation. Both tests documenting the current behaviour are
in `test-config_runner_props.R`, so a future change of precedence will show up there. Fixing it
means reading the environment unconditionally and putting it *after* `properties.json` in the
merge — a behaviour change for every property (a value typed in the Shiny UI would then be ignored
while the variable is set), which is why it is not done as part of this issue.

### Issue 4 — `register_runner()` claims a lease
- [x] `register_runner()` = PID file (unchanged) + `claim_runner_lease()`
- [x] failure message naming the current holder (`host`, since when)
- [x] `conf$runner_name` set
- [x] `release_runner_lease()` on exit in `fs_loop()`, `search_loop_worker()`, `detect_loop()`
- [x] `stop_all_runner_tasks()` still works locally (it must not depend on the lease)
- [x] tests: `tests/testthat/test-runners_register.R`

Diagram updated: `inst/schema/runners_lease.excalidraw`, panel 5 (the two layers of
`register_runner()` and where each of them refuses).

Decided while implementing:

* **The PID file is removed when the claim fails.** `register_runner()` writes it before going to
  the network (the local check is cheaper and disturbs nobody), so a refused claim would otherwise
  leave this machine looking like it hosts a running runner — exactly what `is_*_running()` and
  `stop_all_runner_tasks()` read locally. Order is: PID check → PID write → claim → on failure,
  `remove_running_task_pid()` + `stop()`.
* **`claim_runner_lease()` gained a fast path when the live lease already names me.** It renews
  instead of rewriting, and skips the guard wait: there is nothing to race against, since a
  competing writer would already have replaced my name in the read. Without it,
  `register_fs_monitor()` — called on *every* iteration of the fs monitor's 2-second loop
  (`R/fs.R`) — would pay a JSON write plus a `runner_claim_guard`-second sleep every two seconds.
* **`is_runner_running_locally(name)` added in `R/runners.R`**, and `stop_all_runner_tasks()` uses
  it for `search` and `detect` instead of `is_search_running()` / `is_detect_running()`. Stopping a
  runner means killing a process *here*, so the question is "is there something on this machine to
  kill?", never "is this runner running somewhere in the cluster?". Freezing this now is what keeps
  issue 7 from turning `stop_all_runner_tasks()` into an attempt to kill processes that live on
  another machine. `fs` keeps its `/ping` check, which issue 7 does not touch either.
* **`search_loop()` releases the parent `search` lease too**, though the checklist only named the
  three loops that do the work. Same reason as everywhere else: without it, restarting the search
  service waits a full TTL. This required adding `add = TRUE` to the existing
  `on.exit(parallel::stopCluster(cl))`, which would otherwise have silently replaced it.
* **`conf$runner_name` is set by `register_runner()`, not by `setup_config()`.** Only a process
  that actually claimed a lease may carry one, and `setup_config()` is called again on every
  configuration refresh. It is session state like `instance_id` / `instance_host`, so it is never
  written to `properties.json` — `save_config()` builds its list property by property, so nothing
  had to change there. It stays `NULL` in the Shiny app, which makes `release_runner_lease()` and
  `stop_if_lease_lost()` no-ops there, matching §6.4.
* **The test helper `local_lease_conf()` moved from `test-runners_lease.R` to `helper-conf.R`**, so
  both test files can use it (testthat gives each test file its own environment).

### Issue 5 — Heartbeat during long waits
- [x] `sleep_with_heartbeat(seconds, name)`
- [x] replaces `Sys.sleep()` at `R/search.R:177`, `R/scheduler.R:293`, `R/alert.R:601`
- [x] `renew_runner_lease()` also called once per iteration at the top of each loop
- [x] check that no code path in `search_loop_worker()` can stay silent longer than the TTL
      (rate-limit waits are the case to verify, §3.3)

Diagram updated: `inst/schema/runners_lease.excalidraw`, panel 6 (the two heartbeat shapes, the
three silences they cover and the one they cannot reach).

Decided while implementing:

* **A second helper next to `sleep_with_heartbeat()`: `renew_runner_lease_if_due()`.** Sleeping is
  not the only way a loop stays silent — one round-robin pass of `search_loop_worker()` over many
  topics can take longer than the search TTL without ever sleeping. A heartbeat has to be callable
  from inside such a loop, which a plain `renew_runner_lease()` is not (one HTTP touch per
  request). `if_due` costs one lookup in `lease_last_renewal`, a package environment recording
  when this process last renewed each of its leases, and touches the fs at most once per
  `runner_heartbeat_span`. That record is *local bookkeeping only*: freshness is still decided by
  the mtime on the fs machine, never by what a runner believes about itself. It notes the
  *attempt*, not the success, so a lease that is gone is not re-tested on every iteration — losing
  it is what `stop_if_lease_lost()` is for.
* **The rate-limit waits are the real silence, and `sleep_with_heartbeat()` cannot reach them.**
  They are not a `Sys.sleep()` of ours: `httr2::req_retry()` sleeps *inside* `req_perform()`
  (`R/sm-bluesky-api.R`, `R/sm-mastodon-api.R`), and a Mastodon rate-limit window is 5 minutes —
  exactly `runner_ttl_search`. The only place httr2 gives the control back before sleeping is the
  `after =` callback, so that is where the heartbeat goes: `heartbeat_before_retry(seconds)`,
  called by `bluesky_rerun_after_rate_limit()` and `mastodon_rerun_after_rate_limit()`. A wait
  longer than the heartbeat span is performed there, chopped and with renewals, and `0` is
  returned so httr2 retries immediately; anything shorter — and the `NA` of a transient
  500/502/503 — is returned unchanged, leaving httr2's own exponential backoff (capped at 60 s per
  try, far below every TTL) in place with one renewal before it. Doing the waiting inside a
  callback is unusual enough to be commented at both ends.
* **`fs` could not beat at all — fixed here, though the checklist did not list it.** `fs_loop()`
  claims the lease and then hands the process over to `spark_job()`, which never returns to R, so
  after `runner_ttl_fs` (120 s) the `fs` lease is stale while the service is perfectly healthy.
  This is *not* the multi-machine case — fs runs on exactly one machine by deployment, `search` is
  the service meant to be spread — so the stake is smaller: the lease is the safety net for the
  day somebody points a second machine at the same data directory, and it is what the status table
  of issue 8 reads. Both are worth having right. The fs monitor future is the only part of the fs
  service still running R code, so it now calls `renew_fs_runner_lease()` on its 2-second loop.
  Two things keep this from being the background beater §3.3 rejects: it beats only while the
  service really answers `/ping`, so a wedged fs still lets its lease expire, and the `/ping` call
  itself only happens when a beat is due.
* **The parent `search` lease still goes stale, and that is left as is.** `search_loop()` claims
  `search`, then blocks in `parallel::parLapply()` for the lifetime of the service: there is no
  iteration to hang a heartbeat on. The consequence is cosmetic for now — the Shiny table of issue
  8 will show `search` as stale on a perfectly healthy machine — because exclusivity that matters
  lives on `search.<network>`, which the workers renew. It is the same question as §9.5 (make the
  parent entry presence-only, or drop it), and it should be settled there rather than patched with
  a heartbeat in a process that has nothing left to do.
* **Two `Sys.sleep()` left alone in the loops.** `R/search.R` sleeps 1 second when an iteration
  performed no request, and `R/scheduler.R:44` sleeps 5 seconds before re-reading a `tasks.json`
  it failed to parse. Both are shorter than any heartbeat span, so chopping them would only add
  noise; the renewal at the top of each loop already covers them.
* **`detect_loop()`'s 5-second sleep goes through `sleep_with_heartbeat()` anyway.** Not for the
  renewal — 5 seconds is far below the span — but for the `stop_if_lease_lost()` it brings: an
  idle detect loop then notices a stolen lease while idle, instead of on its next write.

### Issue 6 — Guard the writes
- [x] `stop_if_lease_lost()` before `commit_posts()` / `save_config()` in `search_loop_worker()`
- [x] before POSTing posts in `search_topic()`
- [x] before each `save_tasks()` **call site** in `detect_loop()` (not inside `save_tasks()`)
- [x] before `register_series()` and the geolocation/aggregation triggers
- [x] `episomer_lease_lost` caught at the top of each loop: log, exit cleanly, do not release
- [x] `search_loop()` cluster: one worker exiting on a lost lease must produce a clear log line,
      not the current unexplained `stop()`
- [x] **found while doing issue 5:** the `tryCatch()` wrapping `search_topic()` in
      `search_loop_worker()` re-raises anything that is not `"too-old"` as
      `stop(paste(e$message, e))`, which flattens the condition into a plain error and loses the
      `episomer_lease_lost` class. That path is now reachable: `heartbeat_before_retry()` can
      raise it from inside `req_perform()` during a rate-limit wait. The worker still stops, so
      nothing is corrupted, but it stops with an unexplained error instead of a clean exit —
      re-raise conditions of that class unchanged.
- [x] tests: `tests/testthat/test-runners_guard.R`

Diagram updated: `inst/schema/runners_lease.excalidraw`, panel 7 (the guard points, and the two
ways a runner can end).

Decided while implementing:

* **One helper carries the whole "catch" side: `run_until_lease_lost(expr)`.** It is the mirror of
  `stop_if_lease_lost()`, and having it as a function rather than a `tryCatch()` copied into each
  loop is what keeps the three rules in one place: log an explicit line, return the sentinel
  `"lease-lost"`, and above all **release nothing**. It also documents why the existing
  `on.exit(release_runner_lease())` of the loops did not have to be touched — `release_runner_lease()`
  checks `i_hold_runner_lease()` first and refuses, so it is already a no-op once the lease is gone.
* **The guard in `search_topic()` sits *after* the social media request, not before it.** The
  request itself writes nothing; it only spends one call of somebody else's rate limit. Everything
  after that line writes, and the takeover can perfectly well happen *during* the request, which
  waits minutes on a 429 (`heartbeat_before_retry()`, issue 5). Checking as late as possible is
  therefore both cheaper and stricter.
* **The "geolocation trigger" of §6.3 is not a call of its own.** Geolocation is asked for by the
  `geolocate=true` of the post upload in `search_topic()`, so it is covered by the guard above. The
  aggregation trigger is real and separate: `register_series()` POSTs the series definitions
  (`set_aggregated_posts()`, `R/fs.R`), and is guarded in `search_loop_worker()`.
* **`generate_alerts()` is guarded before it starts, though the checklist only named
  `save_tasks()`.** It is by far the heaviest write of the detect loop — it POSTs the alerts to the
  fs *and* sends emails to the subscribers, so two instances detecting the same period means
  duplicated emails, which no deduplication on the fs side can undo. It is one long block that
  reports its own progress through `update_alerts_task()`, so the guard is at the entry rather than
  sprinkled inside; the residual risk is a task started seconds before a takeover, which the long
  detect TTL (30 min) is designed to make rare. A second guard after it covers the opposite case,
  a task long enough for the lease to be lost while it ran.
* **`search_loop_worker()` now returns a value, and `search_loop()` reads it.** A worker that lost
  its lease returns `"lease-lost"` instead of raising, so `parLapply()` returns normally instead of
  propagating an error, and `report_search_loop_outcomes()` says which of the two things happened:
  every worker stepping aside is the standby mechanism working (log it, return, do not make a
  supervisor restart against a healthy instance), anything else keeps the historical error — now
  naming the networks and what each of them returned, instead of the bare "which is unexpected"
  that sent an operator reading three separate log files.
* **`save_tasks()` deliberately stays unguarded**, and there is a test saying so. The temptation to
  guard the writer itself rather than its call sites comes back on every read of this code; the
  reason it is wrong is that the Shiny app calls it too and holds no lease at all (§6.4), so the
  guard would abort the app as soon as any service took one.
* **The `episomer_lease_lost` re-raise in `search_loop_worker()` is tested first in its handler.**
  Beyond restoring the class, putting it before the `"too-old"` branches keeps it independent of
  `e$message`, which is what the other branches match on.

### Issue 7 — `is_*_running()` reads the lease
- [x] `is_search_running()`, `is_detect_running()`, `missing_search_jobs()` based on the lease
- [x] `is_fs_running()` keeps `/ping`
- [x] local cross-check on the PID when `lease$host == conf$instance_host` (§6.2), which also gives
      the fast reclaim of §3.6
- [x] review the call sites listed in §6.2 (`check.R`, `setup.R`, `shiny-app-utils.R`)
- [x] tests: `tests/testthat/test-runners_running.R`

Diagram updated: `inst/schema/runners_lease.excalidraw`, panel 8 (the two questions — cluster-wide
and local — and who asks which).

Decided while implementing:

* **`is_search_running()` reads the parent lease *and* the per-network ones.** Reading `search`
  alone would have been the obvious translation and it would have been wrong: issue 5 established
  that `search_loop()` claims `search` and then blocks in `parLapply()` with no iteration left to
  renew it, so that lease is knowingly stale on a *healthy* machine after `runner_ttl_search`
  (§9.5). The Shiny app would therefore have shown "Stopped" five minutes after every start. The
  service is running if any of `search` / `search.<active network>` is fresh: the workers really do
  beat, and the just-claimed parent lease covers the startup window before they register. This is a
  workaround for §9.5, not a replacement of it — settling the parent lease question there would
  let this become a single `is_runner_running("search")`.
* **The local cross-check needs two corroborating signals, not one.** §6.2 says "if the lease's
  `host` is my host, cross-check the recorded `pid`"; the host alone is not safe enough. Two
  containers of the same service share `EPISOMER_INSTANCE` (`docker/compose.yml` sets it per
  service, so the standby of issue 9 inherits it) while having separate PID namespaces: "PID 42
  does not exist here" would not mean "the holder is dead", and we would steal a live lease — the
  one thing the whole mechanism exists to prevent. So `lease_holder_is_dead_locally()` also
  requires **this machine's own PID file for that runner to name exactly the PID written in the
  lease**. That file is written by the holder on the disk we are reading (`conf$data_dir` is
  container-local, only `jobs/` is shared), so if it agrees, that PID really is ours to test.
  Everything fails closed: anything missing, unreadable or merely different means "trust the
  mtime".
* **The cross-check lives in `lease_is_free()` and `runner_lease_status()`, not in the
  `is_*_running()` functions.** One implementation then answers both questions at once — "is it
  running?" (issue 7) and "may I claim it?" (the fast reclaim of §3.6) — and issue 8's status table
  gets it for free. `runner_lease_status()` reports such a lease as `stale`, which is what it is
  operationally, plus a `holder_dead_locally` flag so the table can word it "crashed here" rather
  than "not renewed".
* **`setup()`'s services menu now reads the *local* status.** Its two actions,
  `register_*_runner_task()` and `stop_*_runner_task()`, start and kill processes on this machine
  only, so labelling an entry "Stop search loop (currently running)" because another machine holds
  the lease would offer to stop something that is not here. Same reasoning as
  `stop_all_runner_tasks()` in issue 4, and `is_runner_running_locally()` was already there for it.
  A local start that the lease refuses still says so, in the log of the process it spawns.
* **Three small extractions in `R/runners.R`**, so that the lease code tests a PID exactly the way
  the PID layer does: `get_running_task_pid_path(name)`, `read_running_task_pid_file(name)` (which
  keeps "no PID file" and "the recorded process is gone" apart, unlike `get_running_task_pid()`'s
  single `-1`), and `is_r_pid_running(pid)`. Behaviour is unchanged; the former `mac` branch of the
  process test was character for character the same command as the unix one, so the two are now a
  single branch.
* **`check.R` and `shiny-app-utils.R`: reviewed, one wording change.** The app is a pure reader and
  wants exactly the cluster-wide answer (§6.4), so its four fields are unchanged, with a comment
  saying so. `check_all()` is also right to ask the cluster-wide question, but its warnings said
  "the pipeline is not running", which now means something stronger: they say "not running on any
  machine sharing this database". `loop_run_issues()` is deliberately left to issue 8, which owns
  the "running on another host" wording. The Shiny start/stop buttons need nothing: they are always
  both displayed, never gated on the status.
* **Found on the way, unrelated:** `setup()`'s `launch` argument had lost its `@param` in
  `R/setup.R` while `man/setup.Rd` still documented it, so regenerating the documentation silently
  dropped it. Restored in the source.

### Issue 8 — Shiny app: distributed services status
- [x] `runner_leases()` listing everything under `runners/`
- [x] status table: runner, status, host, last seen, since
- [x] `loop_run_issues()` (`R/runners.R:572`) messages updated for the remote case ("running on
      another host" is not the same thing as "not running")
- [x] tests: `tests/testthat/test-runners_status.R`

Diagram updated: `inst/schema/runners_lease.excalidraw`, panel 9 (how the table is built, its five
columns, and when the remote message speaks).

Decided while implementing:

* **The table is built on `runner_lease_status()`, not on the raw JSON.** Issue 7 put the local
  cross-check inside it precisely so that "issue 8's status table gets it for free", and it does:
  a runner whose process crashed on this very machine reads as `crashed here` rather than
  `running`, with no line of its own here. `runner_leases()` therefore returns *statuses* (one per
  lease), and `runner_leases_df()` only turns them into the five display columns.
* **Only live leases have a row, and an empty table is a legitimate answer.** A free runner has no
  file (§3.4), so it has no row: the table says who is *holding* something, not what could be
  started — that is what the three Running/Stopped labels above it are for. `runners/old/` is not
  listed, and the `.json` pattern is what keeps it out of the non-recursive listing (it shows up
  there as a directory entry). When nothing holds a lease, DataTables' `emptyTable` message says it
  in words — "no episomer service is running on any machine sharing this database" — instead of
  showing an empty grid.
* **A row that vanishes between the listing and its read is dropped.** Its holder released it, or
  another instance took over and moved the file; `runner_lease_status()` reports `free` and such a
  row would read as "a runner nobody holds", which is not a thing this table can display.
* **`claimed_at` is now part of `runner_lease_status()`.** It is the "since" column, and it is the
  one field that must come from the JSON rather than from the file: renewing only touches the file,
  so `claimed_at` stays put while `seen_at` moves. A long "since" next to a short "last seen" is
  exactly the picture of a healthy runner, and the two columns are only readable together.
* **"stale" is worded with the TTL it exceeded, and "crashed here" is a status of its own.** Both
  are `stale` for `runner_lease_status()`, but they call for different actions: one is a machine we
  have not heard from (look at it, or wait for the TTL), the other is a certainty about a process of
  ours that is gone and reclaimable right now. Hiding the difference behind one word would waste
  the `holder_dead_locally` flag issue 7 added for this.
* **The host column marks our own machine.** In the single-machine deployment — how episomer runs
  almost everywhere — every row is local, and a bare host name next to a column called "Host"
  leaves the reader wondering whether it is theirs. `machine-A (this machine)` costs one comparison
  with `conf$instance_host` and removes the question.
* **`Since` is displayed in UTC, like the stored value.** The holder may sit in another timezone
  than the reader, and the point of that column is to be comparable with the `runners/*.json` files
  and with the logs of the other machines. Anything unparseable is shown as it was written.
* **The remote message *blocks* the activation, it does not merely inform.** `loop_run_issues()`
  already gates the "activate" buttons through a modal, and spawning a process whose only future is
  to die on `runner_lease_holder_message()` teaches the operator nothing that the modal cannot say
  first — while naming the host, the claim time, the age of the last heartbeat and the TTL, i.e.
  what to do next.
* **`runner_running_elsewhere_message()` is deliberately silent in two cases.** A *stale* lease on
  another machine is what the standby exists for and the claim is going to succeed; a lease held by
  our own host makes starting a local matter, and there the PID check of `register_runner()` gives
  the strictly better message because it names the process to stop. Both are tested, because both
  look like missing checks on a quick read.
* **The check sits at the very top of `loop_run_issues()`, before `get_tasks()`.** Nothing in the
  local task states below could make that start succeed, so asking them first would only produce a
  more precise answer to the wrong question. The runner name of each of the three loops is the loop
  name itself, which is why one line covers `fs`, `search` and `detect`.
* **The table is a status block, not a data table.** `dom = "t"`, no paging, no sorting: the rows
  come in the reading order of `runner_leases_df()` (fs, fs_mon, search, detect, then alphabetical,
  which puts the parent `search` lease right before its `search.<network>` children), and letting a
  click reorder them would only break that. It refreshes with the rest of the `tasks` block of
  `refresh_config_data()`, i.e. every ten seconds: one listing plus one read per runner, the same
  order of magnitude as the four cluster-wide fields the page already computed.
* **Found on the way:** the "activate" button of the alerts service never called
  `loop_run_issues()` at all (`R/shiny-app.R`), so it is the one button that still spawns a doomed
  process when another machine holds `detect`. `loop_run_issues("detect")` now answers the remote
  case, so wiring it in is a two-line change — but it also starts gating a button that was never
  gated, which is a behaviour change this issue does not own. Left for issue 9.

### Issue 9 — Documentation & deployment
- [x] vignette / README section: how to run one fs + N machines
- [x] `docker/compose.yml`: a second `search` service to demo standby, `EPISOMER_INSTANCE` set
- [x] update `inst/schema/services.excalidraw` with the lease layer
- [x] operator guide: how to read `runners/*.json` and what "stale" means

Diagram rewritten from scratch: `inst/schema/runners_lease.excalidraw`, sixteen panels covering the
whole mechanism — why it exists, the vocabulary, the two layers, the file, the runners and their
TTLs, `register_runner()`, the claim, `lease_is_free()`, the heartbeat, the silences it has to
cover, the guard points, the release, the two "is it running?" questions, the Shiny table, the full
two-collector scenario and the accepted limits.

Decided while implementing:

* **The standby container is in a Compose *profile*, not started by default.** `search-standby`
  exists to be watched, not to run: adding a second collector to every `docker compose up` would
  change what the default deployment does, which this issue does not own. `docker compose --profile
  standby up -d` is the whole opt-in.
* **`restart: on-failure` is the standby mechanism.** A refused claim is a `stop()`, so R exits with
  a non-zero status and the container ends; Docker bringing it back is what turns "refused once"
  into "waiting for its turn". Nothing had to be added on the R side — one read of one small JSON
  file per attempt. `on-failure` rather than `always`, so a clean shutdown stays down.
* **The standby gets its own `EPISOMER_INSTANCE`**, though issue 7 established that sharing one
  would still be *safe* (`lease_holder_is_dead_locally()` requires the local PID file to agree
  before declaring a holder dead, precisely because containers have separate PID namespaces).
  Safe is not the same as readable: every message and every row of the status table names that
  value, and a standby that cannot be told apart from the machine it replaces makes the handover
  impossible to follow. The reason is written next to the variable, since the safety argument is
  exactly what makes it look optional.
* **The documentation is one section of the user vignette, not a separate operator file.** The
  checklist lists "vignette / README section" and "operator guide" as two items, but they are read
  in the same situation, by the same person, five minutes apart: *Running episomer on several
  machines* holds the deployment part and ends with *Operator guide: reading the runner leases*
  (the status labels, the JSON fields, and a symptom → meaning → action table). The README gets the
  short version and the pointers. `runners/` was also added to the folder-structure list, which
  claimed to be exhaustive and no longer was.
* **`services.excalidraw` is appended to, not rewritten.** That file is the thinking that *preceded*
  the design — it asks questions ("is the fs_loop allowed to monitor a service on another
  machine?", "when do we check we are allowed to write?"). Rewriting it would have thrown away the
  reasoning and kept only the conclusion. The new section sits below a separating rule and answers
  those questions one by one, then updates the "Services-related functions" list with what changed
  and, as importantly, what did not.
* **Both diagrams are generated by a script** (`devscripts/schema/`, plain Python, no dependency).
  They are far too dense to maintain by clicking, and a diagram that has silently stopped matching
  the code is worse than no diagram. `gen_services_append.py` is idempotent — it recognises its own
  elements by their `index` prefix — so it can be re-run without duplicating anything.
* **The alerts "activate" button is wired here**, as issue 8 left it. Its handler *writes* before
  registering anything: it dates the one-shot tasks and saves the configuration, which the detect
  loop already running on the other machine would pick up immediately. So the `loop_run_issues()`
  check is the very first thing it does, and it returns before touching anything.

### Issue 10 (optional, Scala) — Conditional write in the fs API
- [ ] `POST /file/...?action=upload&ifMd5=<md5>` returning 409 on mismatch
- [ ] `?action=delete`, if `runners/old/` ever turns out to be a nuisance — with `move` (§3.4) it
      is no longer needed for the release path
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

- [x] free lease (absent file) → claim succeeds
- [x] fresh foreign lease → claim fails, the holder is reported
- [x] expired foreign lease → claim succeeds (mock `epifile.mtime()` to fake age, or
      `Sys.setFileTime()` in local mode — the latter is a real test of the mtime path)
- [x] released foreign lease (file moved to `runners/old/`) → claim succeeds regardless of mtime,
      and the released file is still readable under `runners/old/`
- [x] `release_runner_lease()` on a lease stolen in the meantime → the holder's file is left alone
- [x] my own lease → `i_hold_runner_lease()` TRUE, `stop_if_lease_lost()` silent
- [x] stolen lease → `stop_if_lease_lost()` raises a condition of class `episomer_lease_lost`
- [x] `renew_runner_lease()` moves the mtime forward and does **not** change the content
- [x] per-service TTL: `runner_lease_ttl("search.bluesky")` → `conf$runner_ttl_search`
- [x] (issue 5) `sleep_with_heartbeat(2)` with `runner_heartbeat_span = 1` renews at least once (mock
      `renew_runner_lease()` and count calls — do not sleep for real in the suite). Done with
      tenths of a second rather than no sleep at all: the span is read from `conf`, so a test makes
      a "long" wait last 0.3 s, which keeps the real `Sys.sleep()` path — the thing being tested —
      instead of mocking a base function testthat refuses to mock. Same section covers
      `heartbeat_before_retry()` and `renew_fs_runner_lease()`.
- [x] (issue 6) in `tests/testthat/test-runners_guard.R`: `run_until_lease_lost()` returning the
      loop's own value, turning a lost lease into `"lease-lost"` with a log line and leaving the new
      holder's file alone, and still letting a real error through; `search_topic()` refusing to
      upload after a takeover that happens *during* the social media request, and uploading normally
      while the lease is held; `search_loop_worker()` returning `"lease-lost"` across the
      `tryCatch()` that used to flatten the condition, and still failing loudly on a real error;
      `report_search_loop_outcomes()` on both of its cases; and `save_tasks()` staying unguarded for
      the Shiny app.
- [x] (issue 8) in `tests/testthat/test-runners_status.R`: `runner_leases_df()` empty but with its
      five columns when nothing holds a lease, one row per live lease in reading order, the
      released lease under `runners/old/` and a stray non-JSON file both ignored, the wording of
      `stale` vs `crashed here`, the "last seen"/"since" pair on an aged lease,
      `format_lease_duration()` on its four scales plus a negative age, and
      `runner_running_elsewhere_message()` / `loop_run_issues()` naming the remote host while
      staying silent on a stale foreign lease and on a lease of our own host.
- [x] (issue 7) `is_search_running()` on a stale lease → FALSE; on a fresh one → TRUE. In
      `tests/testthat/test-runners_running.R`, together with: a fresh *foreign* lease reading as
      running with no PID file on this machine at all (the point of the issue), the split of §7 case
      7 through `missing_search_jobs()`, the healthy-but-stale parent `search` lease, the crashed
      local holder reading as stopped and reclaimable while its mtime is still fresh, the three
      cases where the cross-check must *not* fire (live local process, no corroborating PID file,
      another host), and `is_fs_running()` answering FALSE on `/ping` while holding a fresh `fs`
      lease.

Also covered, and the helper the plan suggested: `with_instance()` lives in
`tests/testthat/helper-conf.R`, the claim race of §3.1 case 3 turned out to be unit-testable
after all — `runner_claim_wait()` exists as a seam so a test can make a competing write land
exactly during the guard wait — and `epifile.move()` / `to_remote_dest()` are covered in
`test-fs.R` and `test-paths.R`.

Kept for a manual/integration check on `docker compose`: everything going through the fs API
over HTTP (only the `fs_remote = NULL` branch is unit-tested) and the takeover after killing a
container (case 5). Those are written down as a runnable checklist, with the file, log line or
table row to look at for each of them, in `inst/schema/multi_services_manual_tests.md`.

### Issue 11 — A failed request must not kill the collection of a network

Not planned: found on the **first real run** of the whole thing, with the three services started
one by one from the `setup()` wizard. Everything below was read from `EPI_HOME` after the fact.

Symptom: `runners/search.mastodon.*.json` were being renewed every twenty seconds while
`search.bluesky` had already been moved to `runners/old/`, six seconds after the service started.
`jobs/search.bluesky.log` stopped in the middle of the collection, on the line announcing its very
first request, with **no error after it**. The worker process was still alive — a PSOCK node idling
in `select()`, waiting for a command from its parent — while mastodon collected next to it for
hours as if nothing had happened.

What actually happened, and why none of it was visible:

* `search_topic()` raised on the first bluesky request. The `tryCatch()` of `search_loop_worker()`
  re-raised anything that was not `"too-old"`, so **one failed request ended the collection of that
  network** until somebody restarted the whole service;
* the error text was lost. A PSOCK node hands its errors back to the parent process, which reads
  them only once **every** node has returned — and the other networks are still looping, so never.
  `report_search_loop_outcomes()` (issue 6) is unreachable in that case: it is the *return* path,
  and the crash path never reaches it;
* the `on.exit(release_runner_lease())` of issue 4 ran and did its job, which is why the lease was
  in `runners/old/`. The PID file `search.bluesky.PID` stayed behind though, still naming a live
  process (the idling node) — so the two exclusion layers answered differently about the same
  runner: "free" for the cluster, "running here" for this machine.

- [x] `try_search_topic()`: one request, three outcomes (`done` / `too-old` / `failed`), returned
      instead of raised. `episomer_lease_lost` still goes straight through — it is not a request
      that went wrong, and retrying it would mean writing from an instance that is not the runner
- [x] the worker absorbs a failed request, logs it, waits (exponential backoff capped at five
      minutes, through `sleep_with_heartbeat()`) and tries again
- [x] `search_max_request_failures()` consecutive failures end the worker, so that a permanent
      failure releases the lease instead of pretending to collect
- [x] `search_loop()` catches what a worker raises, writes it in that network's log and turns it
      into an outcome `report_search_loop_outcomes()` can name
- [x] the node restores its `sink()`s on exit
- [x] `unregister_runner()` = release the lease + drop the PID file, used by the `on.exit()` of the
      four loops
- [x] tests: `tests/testthat/test-search_retry.R`, plus `unregister_runner()` in
      `test-runners_register.R`

Decided while implementing:

* **Retrying for ever is not the safe default it looks like.** A wrong credential or a query the
  API rejects fails identically every time, and a worker that keeps retrying keeps renewing its
  lease: the service would look healthy while collecting nothing, which is strictly worse than the
  crash it replaces. Giving up after ten consecutive failures (~20 minutes with the backoff)
  releases the lease, which is what puts the network back in `missing_search_jobs()` and in the
  status table of issue 8. The cluster-wide visibility built by issues 7 and 8 *is* the alerting
  mechanism here — but only if a dead runner stops holding its lease.
* **The counter is per worker and reset by any success**, not per plan. A network that answers
  answers for every topic, and a flaky one must never accumulate its way to the cap. The known
  cost: a single topic failing while the others succeed keeps being retried ahead of them
  (`plan$requests` does not move on a failure, so the round robin comes back to it), starving the
  others until the cap. Bounded, and the common case — the whole network being down — is unaffected.
* **The backoff waits through `sleep_with_heartbeat()`**, like every other long wait since issue 5.
  Five minutes of plain `Sys.sleep()` is exactly the search TTL: a runner would hand its network to
  a standby machine while it was merely waiting for an API to come back, and wake up collecting
  behind the new holder.
* **`search_max_request_failures()` and `search_retry_wait()` are functions, not `conf`
  properties.** Issue 3 established where deployment knobs go, but these two are not knobs yet:
  nobody has needed a different value, and a function is what a test can shorten. Promoting them to
  `conf` is purely additive if that changes.
* **Found on the way:** the `"too-old"` recovery of the first plan called `search_topic()` without
  passing `sandboxed` on, so a sandboxed run recovering from a too-old request uploaded posts after
  all. Fixed with a test.
* **Still true, and not fixed here:** the parent process cannot notice a worker that returned while
  the others keep looping — that is `parLapply()`'s scheduling, and changing it means changing the
  parallelism model. What makes a dead network visible now is the log of that network, plus the
  released lease showing up in `missing_search_jobs()`. Worth revisiting if the search service ever
  gets a supervisor.
