# fedstatAPIr 2.0.0

## New: reliable downloads in parts

* `fedstat_download()` downloads an indicator in small parts, one request at a
  time with pauses, and saves every finished part to disk. Fedstat builds every
  combination of the selected filter values, so large selections are split into
  parts of at most `max_nominal_cells_request` combinations (100,000 by default).
  If a run stops (time allowance, Fedstat outage, closed R), running the same
  call again continues with the remaining parts.
* Failed requests are retried; a part is split further only when its size is
  the likely cause. Outages and access refusals lead to recovery pauses instead.
* If Fedstat renames or withdraws selected values during a download, the
  download continues with a warning. It stops only if a whole filter is added or
  removed, or an export contains values that were not requested.
* `preview_first = TRUE` asks Fedstat to show each part's table before
  downloading it, like pressing "Apply" on the website.
* `fedstat_collect()` builds the table from a finished download. `ObsValue` is
  numeric by default; if a value is not a number, it stops and names it
  (`try_to_parse_ObsValue = FALSE` returns text).
* `fedstat_cleanup()` deletes the saved parts after you have saved the table.
* Each download keeps a diagnostic `download.log` without tokens, cookies or
  response bodies.

## Deprecated

* `fedstat_data_load_with_filters()` still works but warns on every call (class
  `fedstat_deprecated`). It sends the whole selection as one request, which is
  slow for large selections and heavy for Fedstat. Use `fedstat_download()` and
  `fedstat_collect()` instead; see `MIGRATION.md`.

## Changes in existing functions

All 1.x functions keep their names, arguments and defaults.

* `try_to_parse_ObsValue = TRUE` now reliably raises an error for values that
  are not numbers; in 1.x this check could silently pass.
* An export that Fedstat marks as truncated is an error, not complete data.
* `retry_max_times` in `fedstat_data_load_with_filters()` counts whole attempts;
  retries are no longer nested and permanent errors stop immediately.
* A missing or expired download token is renewed automatically, with the same
  HTTP settings.
* httr settings passed in `...` apply to every request, including token
  renewal. `httr::set_config()` and `options(fedstatAPIr.config = ...)` are
  respected.
* Errors have classes (all inheriting from `fedstat_error`) with the failed
  step in `e$stage` and the HTTP status in `e$status`. Messages no longer include
  response bodies.
* Indicator pages are read more robustly, indicator codes are no longer limited
  to digits, and SDMX parsing accepts files, uses less memory and rejects
  damaged exports.

## Documentation

* New Russian and English READMEs for researchers: a quick example with real
  data, why downloads go in parts, and what to do about each message.
* `MIGRATION.md` explains upgrading from 1.x.

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