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