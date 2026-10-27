# fedstatAPIr (development)

* Empty Cartesian parts now complete after an export HTTP 302 and an explicit
  zero-row preview. Direct downloads query the preview only when needed.
* Unreleased batch arguments are `cache_dir`, `total_time_limit_seconds` and
  `prepare_before_download`; earlier names produce migration guidance.
* Indicator identifiers are accepted as opaque codes rather than digits only.
* Russian and English READMEs focus on downloading, collecting and resuming;
  implementation details remain in the reference and technical notes.

* Optional `prepare_before_download = TRUE` applies each part's filters to the browser
  preview before SDMX export, sharing the session, pauses and time allowance.
* Progress reports completed nominal combinations, including after subdivision
  and resume. Retry messages identify the failed step; filter readiness is explicit.
* Sources use styler with four-space indentation.

* Batch downloads now default to a one-hour time allowance, without an HTTP-count
  ceiling. Requests and pauses share it; expiry preserves resumable progress.
* The batch size control is `max_nominal_cells_request` (default 100,000).
  Values above 250,000 warn; the earlier unpublished size names are removed.
* Console output defaults to short progress messages; `verbose = "detailed"`
  exposes technical events while diagnostic file logs always retain full detail.
* Suspected access checks exclude script/style/comment text. Batch jobs warn,
  wait at least five minutes, and refresh the token within bounded recovery.
* `fedstat_cleanup()` and `fedstat_collect(remove_parts = TRUE)` explicitly remove
  verified parts while retaining logs and unrelated files.

* Older curl callback-abort errors now propagate as interrupts, with no retry or
  network-error report. The price-check script exits cleanly on cancellation.
* Response sniffing handles UTF-8 characters cut by the inspection window without
  spurious warnings or changes to the downloaded data.

* Batch downloads recover automatically after bounded cooldowns, separately retry
  transient token GETs, and subdivide ambiguous export rejections after session
  repair. Recovery shares the original invocation time allowance.
* Warnings are included in diagnostic logs; stop messages distinguish exhausted
  budgets from persistent failures.
* New downloads store data in `parts/`. Existing flat checkpoints resume unchanged.
* The main README is fully Russian and leads with an explained batch example;
  English documentation is provided separately.

* Batch downloads now append timestamped JSON Lines to `download.log`, independent
  of console verbosity. Events include request counts/stages, waits, timings,
  byte counts, token refreshes, split dimensions/reasons and saved progress.
* All failures and interruptions after job setup receive a recovery summary.
* Completion now includes disk-based validation of keys across parts; duplicate
  observations cannot be deferred until collection. Older manifests are validated
  once without downloading again. Equivalent filter orderings and label forms
  are accepted on resume.
* `tools/prices.R` provides a bounded real CPI workload with diagnostic files.

* Existing exported function names, arguments, defaults and successful return
  types are preserved. No call-site migration is required.
* Added `fedstat_download()` for sequential, resumable downloads, with explicit
  request-size, token-reuse, retry and time-allowance controls. Verified RDS
  parts require no additional packages; `fedstat_collect()` loads them.
* HTTP configuration now reaches fallback token GETs. Global httr configuration
  and per-call headers override package defaults. Existing timeouts still bound
  the complete HTTP transfer; connection and low-speed limits can be set in `...`.
* Retry ownership is explicit: the convenience wrapper no longer multiplies GET
  retries or retries permanent errors. Resolved metadata survive session refresh.
* Added typed errors with stage and HTTP status fields. Error text omits response
  bodies, cookies and tokens. See `MIGRATION.md` for custom retry wrappers.
* Metadata scripts are located by content rather than position. Numeric parsing
  now detects invalid values even when the source contains no missing values.
* SDMX parsing avoids a full text copy, cleans temporary files on failure,
  accepts file input, skips observation parsing for dictionary-only requests,
  and rejects truncated or structurally invalid exports.
* Added dependency-free offline regression tests and opt-in live/benchmark scripts.

# fedstatAPIr 1.1.0

## Breaking changes

* Unspecified filter fields with multiple possible values now produce an informative error instead of silently selecting all values. Use `"*"` to explicitly select all values for a filter field. Single-value-only filters are still auto-selected. The error message lists all unspecified fields with an example of how to fix them.
* Minimum R version raised from 2.10 to 3.5.0 (required by serialized data format).

## Bug fixes

* EMISS moved data download endpoint from `/indicator/data.do` to `/indicator/downloadData.do`. All download functions updated automatically -- no user code changes required.
* Fixed pre-existing typo `ObsValues` -> `ObsValue` in `fedstat_parse_sdmx_to_table()` that caused the numeric parsing safety check to silently never fire.

## New features

* `fedstat_data_load_with_filters()` now automatically retries the full download pipeline (GET + filter + POST) on transient failures, re-fetching a fresh CSRF token on each attempt.
* CSRF token is automatically extracted from the indicator page during `fedstat_get_data_ids()` and passed through the pipe chain via data.frame attributes.
* HTTP session (cookies) are now persisted between the page GET and data POST via `httr::handle()`, also passed as a data.frame attribute.
* Improved error messages: specific diagnostics for HTTP 302 (rejected filters), 403 (anti-bot), 503 (overloaded), HTML-instead-of-data responses, and CSRF token failures.
* `fedstat_parse_sdmx_to_table()` now detects HTML error pages before attempting XML parsing, giving clearer error messages.

## Other changes

* Honest User-Agent header (`fedstatAPIr/1.1.0`) is now set by default. Users can override via `...` arguments or `httr::set_config()`.
* `fedstat_post_data_ids_filtered()` no longer retries POST requests internally (CSRF tokens are single-use). Use the wrapper `fedstat_data_load_with_filters()` for automatic retries, or re-run from `fedstat_get_data_ids()` when using individual functions.
* Refreshed `fedstat_indicators_names_database` (5224 indicators, new schema: department, group, id, title, hidden).

# fedstatAPIr 1.0.0

* removed all dependencies except httr, jsonlite, xml2, readsdmx, magrittr, utils, methods, data.table for much easier installation
* backend of all functions rewritten in data.table
* new features: it's now possible to load database full dictionary from sdmx by providing new argument `return_type` to `fedstat_parse_sdmx_to_table`, obervation value in data is now parsed from character to numeric type automatically
* fixed some old problems with russian encoding data fields and filters by converting to UTF-8
* removed all functions related to automatic data update checks (`fedstat_check_data_update`, `fedstat_prepare_reference_data_for_check_data_update`), were experemental, too complex and not a main part of a package
* removed function `fedstat_get_data_ids_special_cases_handle`, no longer needed due to change of upstream API
* new function `fedstat_indicator_info` - allows to update database of all indicators by user
* user interface to the package (except deleted functions) has not changed, some non-significant default arguments have been changed or added