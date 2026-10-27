# Compatibility and recovery

The six previously released functions retain their names, arguments, defaults
and successful return types. The new batch downloader uses the time allowance
described below.
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
| `fedstat_blocked` | HTTP 403 or suspected HTML access check/refusal | Batch downloads warn, cool down and refresh; bounded by recovery count and time |
| `fedstat_unexpected_response` | HTML or unexpected media | Batch jobs may probe smaller parts after one session repair; never assume empty data |
| `fedstat_metadata`, `fedstat_parse` | Unexpected page/export structure | Inspect; do not subdivide |
| `fedstat_truncated` | Source explicitly reports truncation | Subdivide within the time allowance |
| `fedstat_budget`, `fedstat_unavailable` | Bounded job stopped | Progress remains; resume later |

All these inherit from `fedstat_error` and `error`. `stage` distinguishes
`metadata`, `token`, `grid`, `export`, `parse` and `schedule`. HTTP errors include `status`;
rate-limited/unavailable responses may include a parsed `retry_after` in seconds.
Normal R programming, argument and storage errors remain normal errors.

## Cancellation and console output

Cancellation is an R `interrupt`, including the callback-abort errors emitted by
older curl versions. It bypasses `error` handlers and never triggers retries.
The downloader logs `INTERRUPTED` and releases its lock while retaining saved
parts. Scripts can use `tryCatch(..., interrupt = function(e)
quit(save = "no", status = 130, runLast = FALSE))` to exit without an error report.
The `curl_interrupt` option is enabled during requests and restored on exit.

## Time allowance and request size

`fedstat_download()` now defaults to `total_time_limit_seconds = 3600` (one hour),
with no HTTP-count ceiling. Requests, pauses, retries and local processing share this allowance.
Requests are capped by remaining time; required pauses are never shortened to
fit. Local parsing, validation and saving finish safely and may extend elapsed
time. Expiry raises `fedstat_time_limit`, retaining verified parts. Repeat the
same call to resume with a fresh allowance; do not change the output folder.

The request-size argument is **`max_nominal_cells_request`**, with a default of
100,000 nominal Cartesian cells. The earlier unpublished names `max_cells` and
`max_combinations` are removed; rename the argument when updating a development
script. Values above 250,000 warn immediately, including in quiet mode. This is
an advisory threshold, not a server capacity guarantee.

`tools/prices.R` uses the same one-hour allowance. Logs include
`total_time_limit_seconds`, `remaining_seconds` and `max_nominal_cells_request`.
Console output defaults to short progress updates. `verbose = FALSE`/`"quiet"`
hides updates, while `verbose = "detailed"` prints diagnostic events. `TRUE`
means `"progress"`. JSON logs retain full events independently. Warnings use
ordinary R conditions and can be handled with `withCallingHandlers`; options,
output sinks and console state are not changed by the package.

Suspected HTML access checks now exclude script/style/comment text and explicitly
hidden elements. Fixed evidence labels are logged, never HTML contents. This is
still heuristic detection, not proof of a CAPTCHA. Batch jobs warn and wait
`access_pause = 300` seconds before retrying with a refreshed token. Repeated
refusals double the wait and share `recovery_max_times` and the time allowance.
HTTP 403 uses the same cooldown path; Retry-After is respected. Access refusals
never cause splitting. The lower-level functions still return typed errors.

`fedstat_collect(x, remove_parts = TRUE)` removes only manifest-listed parts after
successful collection and validation. For a durable result, save the table first
and call `fedstat_cleanup(x)` separately. Both operations lock the job directory.
Cleanup retains the log, a manifest marked `cleaned`, and unrelated files; a
cleaned job cannot be resumed or recollected. Interrupted cleanup can be repeated.

No saved-file migration or redownload is needed. For the Windows R 4.3 launcher
caveat, see [tools/README.md](tools/README.md).

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

Use the same indicator, filter specification and cache_dir to resume. Completed
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
POST, including retries and cooldown recovery, shares the invocation budget
(time by default since 1.1.0.9004).

Two additive arguments control automatic recovery: `recovery_max_times = 3L`
and `recovery_pause = 60`. After ordinary attempts are exhausted, recoverable
failures wait at least 60, 120, then 240 seconds before trying unfinished work
again. A saved part resets this counter. Set `recovery_max_times = 0` for the
previous stop-after-retries behavior. No recovery is performed on cancellation,
time/subdivision budget exhaustion, invalid filters, parse
errors or storage errors. HTTP 429 respects Retry-After and never causes splitting.

An ambiguous export redirect/HTML response rejected again after a token repair
can now trigger subdivision. Explicit CSRF failures and token/metadata GET errors
cannot. `max_failures` now counts ambiguous export rejections as well as transient
transfer errors; reaching it triggers a bounded cooldown instead of more splitting.
Persistent refusals still stop; redirects alone are never accepted as empty data. Explicit preview confirmation is described below.
`RECOVER`, `WARNING` and more specific stop advice are written to the log.

New manifests include `parts_dir = "parts"`; their data files go into `parts/`.
Older manifests without that field keep their flat layout. No manual migration
or redownload is needed, and the directory-level API is unchanged. Always use
the paths recorded in `manifest.rds` rather than assuming a particular file layout.

## Batch argument names (1.1.0.9008)

These names belong to the unreleased batch API. Update existing development scripts:

| Earlier name | Current name |
| --- | --- |
| `path` | `cache_dir` |
| `time_limit_seconds` | `total_time_limit_seconds` |
| `prepare_grid` | `prepare_before_download` |

Old names raise an early error listing the replacements; they are not silently
passed to httr. Reuse the same directory: no checkpoint migration or redownload
is needed. The result's `$path` field remains unchanged. Logs now use
`total_time_limit_seconds` and `prepare_before_download`. The six released
function signatures are unchanged. Indicator IDs are opaque nonempty codes;
numeric input is still accepted for convenience. URL components are encoded.

## Preparation and empty selections

`prepare_before_download = TRUE`, immediately after `token_policy`, applies each
part's filters to `dataGrid.do` before every SDMX attempt on the same session.
Some server states may require this sequence. It adds a data request and shares
pacing, configuration and total time; it is disabled by default. Nonempty preview
observations are never substituted for the SDMX result.

An export HTTP 302 triggers an exact-selection preview check when preparation is
disabled. When enabled, the current attempt's preview is reused. A JSON object
containing exactly `results: []` and `__count: 0` (or `"0"`) confirms an empty
part. Missing, contradictory or malformed fields are errors; redirects, access
refusals and timeouts alone never establish emptiness. The saved zero-row RDS has
an `ObsValue` character column; its manifest records
`empty_evidence = "preview_zero_after_export_302"`. Other columns come from
nonempty SDMX parts. An entirely empty download collects to a zero-row table
with only `ObsValue`, since there is no SDMX schema to preserve.

Empty parts count toward nominal progress, survive resume and undergo the same
checksum/cleanup handling as other parts. The preview is an upstream assertion
of no observations, not independent verification of Fedstat's database.

`percent_selected` sums completed Cartesian regions against the frozen selection;
split parents are excluded. It works with old checkpoints and is not an estimate
of output rows or remaining time.
