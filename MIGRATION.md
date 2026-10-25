# Compatibility and recovery

No existing exported function was renamed or removed. Existing arguments,
defaults, positional calling conventions and successful return types remain.
The new `fedstat_download()` and `fedstat_collect()` functions are optional.

## Existing pipelines

Continue using `fedstat_get_data_ids()` → `fedstat_data_ids_filter()` →
`fedstat_post_data_ids_filtered()` → `fedstat_parse_sdmx_to_table()`, or the
one-call `fedstat_data_load_with_filters()` wrapper. Preserve the session
attributes when manually subsetting `data_ids`; the filter helper does this.
Metadata and session state now have separate internal lifetimes. A token refresh
keeps the resolved selection. Saved session handles are not portable across R
processes: fetch fresh IDs when starting a new process.

There are deliberate corrections to failure behavior:

* `retry_max_times` on the convenience wrapper bounds pipeline attempts. It no
  longer nests another retry loop within each attempt. Permanent errors stop
  immediately, with progress messages explaining retries and session recovery.
* Invalid numeric source markers consistently raise an error when
  `try_to_parse_ObsValue = TRUE`. Use `FALSE` to retain raw values such as `"..."`.
* Truncated XML exports raise an error instead of being treated as complete data.
* Response bodies are no longer included in errors, because they may contain tokens.
* A missing token is refreshed with the same per-call HTTP settings; the previous
  missing-token warning is no longer emitted for this normal recovery operation.

## Custom retry code

Ordinary `tryCatch(..., error = ...)` still works. Prefer condition classes and
`e$stage` over regular expressions over error messages:

| Condition class | Meaning | Suggested response |
| --- | --- | --- |
| `fedstat_token`, `fedstat_redirect` | Rejected token or ambiguous redirect | Refresh once; batch jobs may probe smaller parts after repeated ambiguous export rejection |
| `fedstat_transient` | Temporary HTTP/transport failure | Back off; inspect `e$stage` |
| `fedstat_rate_limit` | HTTP 429 | Honor `e$retry_after`; do not subdivide |
| `fedstat_blocked` | HTTP 403 or access challenge | Stop |
| `fedstat_unexpected_response` | HTML or unexpected media | Batch jobs may probe smaller parts after one session repair; never assume empty data |
| `fedstat_metadata`, `fedstat_parse` | Unexpected page/export structure | Inspect; do not subdivide |
| `fedstat_truncated` | Source explicitly reports truncation | Subdivide within the request budget |
| `fedstat_budget`, `fedstat_unavailable` | Bounded job stopped | Progress remains; resume later |

All these inherit from `fedstat_error` and `error`. `stage` distinguishes
`metadata`, `token`, `export`, `parse` and `schedule`. HTTP errors include `status`;
rate-limited/unavailable responses may include a parsed `retry_after` in seconds.
Normal R programming, argument and storage errors remain normal errors.

## Cancellation and console output

Cancellation is an R `interrupt`, including the callback-abort errors emitted by
older curl versions. It bypasses `error` handlers and never triggers retries.
The downloader logs `INTERRUPTED` and releases its lock while retaining saved
parts. Scripts can use `tryCatch(..., interrupt = function(e)
quit(save = "no", status = 130, runLast = FALSE))` to exit without an error report.
The `curl_interrupt` option is enabled during requests and restored on exit.

Console output now uses `[HTTP 1; limit 40]`: 40 is the invocation's request
ceiling, not a planned part count. JSON log field names remain unchanged.
Changing `max_cells` does not change that ceiling or merge existing partitions.
For the Windows R 4.3 launcher caveat, see [tools/README.md](tools/README.md).

## Network settings

Per-call configuration applies to every request, including token refreshes:

```r
fedstat_data_load_with_filters(
  "37089", filters = my_filters,
  timeout_seconds = 300,
  httr::user_agent("my-project/1.0"),
  httr::config(connecttimeout = 20, low_speed_limit = 1, low_speed_time = 60)
)
```

`timeout_seconds` bounds the whole HTTP operation, including a transfer that
continues making progress. `connecttimeout` bounds connection establishment;
`low_speed_time` bounds a sustained stall. A server that spends a long time
computing before sending bytes can trigger a low-speed timeout too. Choose it
accordingly. Cancellation does not guarantee cancellation of server-side work.

Existing `httr::set_config()` settings are respected. Package-specific defaults
can also be supplied with `options(fedstatAPIr.config = httr::config(...))`;
per-call settings take precedence. No browser impersonation is required by the
package. Token reuse is configurable on the batch API because one successful
reuse does not establish a universal token lifetime.

## New batch results

`fedstat_download()` returns a small result describing files, not a giant data
frame. Call `fedstat_collect()` only when the full result fits in memory, or read
the completed part files named in `manifest.rds` individually with `readRDS()`.
Raw values and codes are retained as strings. Existing parser defaults are
unchanged; raw values are the explicit contract of this new batch API.

Use the same indicator, filter specification and path to resume. Completed
parts are checksummed; corrupt or missing parts stop with an actionable error.
Changing a selection requires a new path. Request sizes, pauses and budgets can
be adjusted on resume. Unlike the legacy filtering helper, the batch function
rejects partially unmatched filter values instead of proceeding after a warning.
No research-specific publication cutoff or monthly-period assumption is applied.

## Diagnostic logs and completion validation (1.1.0.9001)

No existing arguments or defaults were removed. The additive `log_file` argument
on `fedstat_download()` defaults to `file.path(path, "download.log")`. Every run
appends JSON Lines with a unique run identifier and UTC timestamps. `verbose`
controls the console only; use `log_file = NULL` to disable disk logging.
Budgets, HTTP stages, waits, response timings/bytes, token refreshes, split
reasons and final recovery summaries are recorded. Headers, session credentials
and response bodies are not logged. `tools/prices.R` also saves session details.

The manifest gains a `validated` flag. A run is complete only after all parts
pass integrity and cross-part duplicate-key checks. Existing manifests lacking
this flag are validated once, offline, when resumed; no data are redownloaded.
Validation uses temporary key buckets in the job directory and needs extra disk
space. Errors and normal cancellation clean these temporary files. After a hard
process termination, leftover `.fedstat-validation-*` directories can be removed
once no writer is active; saved part files must be retained.

Resume now accepts reordered fields/values, case/whitespace variations, numeric
versus character labels and duplicate identical values. It still rejects changes
to the requested scope. The global `"*"` spelling and a named list remain distinct
specifications; keep the original form when resuming wildcard jobs.

## Automatic recovery and folder layout (1.1.0.9002)

The existing six public functions retain their signatures and defaults. On the
new batch API, `retry_max_times` still bounds export attempts before subdivision
or cooldown. A transient token GET now has its own retry allowance, so a failed
refresh does not necessarily consume the last export opportunity. Every GET and
POST, including retries and cooldown recovery, remains inside `max_requests`.

Two additive arguments control automatic recovery: `recovery_max_times = 3L`
and `recovery_pause = 60`. After ordinary attempts are exhausted, recoverable
failures wait at least 60, 120, then 240 seconds before trying unfinished work
again. A saved part resets this counter. Set `recovery_max_times = 0` for the
previous stop-after-retries behavior. No recovery is performed on cancellation,
request/subdivision budget exhaustion, access refusal, invalid filters, parse
errors or storage errors. HTTP 429 respects Retry-After and never causes splitting.

An ambiguous export redirect/HTML response rejected again after a token repair
can now trigger subdivision. Explicit CSRF failures and token/metadata GET errors
cannot. `max_failures` now counts ambiguous export rejections as well as transient
transfer errors; reaching it triggers a bounded cooldown instead of more splitting.
Persistent refusals still stop; redirects are never accepted as empty data.
`RECOVER`, `WARNING` and more specific stop advice are written to the log.

New manifests include `parts_dir = "parts"`; their data files go into `parts/`.
Older manifests without that field keep their flat layout. No manual migration
or redownload is needed, and the directory-level API is unchanged. Always use
the paths recorded in `manifest.rds` rather than assuming a particular file layout.
