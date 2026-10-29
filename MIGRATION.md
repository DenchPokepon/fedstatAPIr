# Upgrading from fedstatAPIr 1.x

Version 2.0.0 adds `fedstat_download()`: it downloads in small parts, pauses
between requests and continues after an interruption. Use it for new work.
Existing 1.x scripts keep working without changes.

## Switch to `fedstat_download()`

```r
# 1.x: one request for the whole selection
data <- fedstat_data_load_with_filters("31074", filters = filters)

# 2.0: small parts, saved progress
job <- fedstat_download("31074", filters = filters, cache_dir = "cpi-download")
data <- fedstat_collect(job)
```

The `filters` list is the same. Differences:

* `cache_dir` is a folder for saved parts. If a run stops, run the same call
  again and only unfinished parts are downloaded. Each run has one hour by
  default (`total_time_limit_seconds`). Without `cache_dir`, a folder named after
  the indicator and the filters is created in the current folder.
* `fedstat_collect()` returns `ObsValue` as numbers by default, like
  `try_to_parse_ObsValue = TRUE` in 1.x. If a value is not a number, it stops
  and names the values; use `try_to_parse_ObsValue = FALSE` for text.
* A filter value that matches nothing on Fedstat is an error, not a warning.
* `fedstat_cleanup(job)` deletes the saved parts after you have saved the table.

See `?fedstat_download` for request size, pauses, retries and what happens
when Fedstat changes its filters during a download.

## What still works

All 1.x functions keep their names, arguments and defaults, but data requests
are now limited in size (see below):

* `fedstat_data_load_with_filters()` still works but is **deprecated**. It
  sends the whole selection as one request, which is slow for large selections
  and heavy for Fedstat. Up to 50,000 filter combinations it shows a message
  once per session; above that it warns on every call. Both have class
  `fedstat_deprecated` if you need to handle them.
* `fedstat_get_data_ids()`, `fedstat_data_ids_filter()`,
  `fedstat_post_data_ids_filtered()` and `fedstat_parse_sdmx_to_table()` remain
  the building blocks for advanced use. Keep the attributes of `data_ids` when
  subsetting it by hand (the filter helper does this): they carry the download
  session. Fetch new IDs in each R session; saved sessions do not carry over.
* `fedstat_indicator_info()` is unchanged.

## Request size limits

A request's size is its number of filter combinations: the product of the
number of selected values of every filter. Fedstat checks every combination,
even empty ones, so huge requests rarely succeed and slow Fedstat down for all
its users. `fedstat_data_load_with_filters()` and
`fedstat_post_data_ids_filtered()` therefore:

* warn above 50,000 combinations;
* stop above 100,000, unless you add `allow_large_requests = TRUE`;
* always stop above 250,000. Use `fedstat_download()` for such selections.

The error has class `fedstat_too_large`; nothing is sent to Fedstat.

## Behaviour corrections in 1.x functions

* `try_to_parse_ObsValue = TRUE` now reliably raises an error for values that
  are not numbers. In 1.x this check could silently pass. Use `FALSE` to keep
  values such as `"..."` as text.
* An export marked as truncated by Fedstat is an error, not complete data.
* `retry_max_times` in `fedstat_data_load_with_filters()` counts whole attempts;
  retries are no longer nested. Permanent errors stop immediately.
* A missing or expired download token is renewed automatically with the same
  HTTP settings.
* Error messages no longer include response bodies, which may contain tokens.

## Handling errors in scripts

`tryCatch(..., error = ...)` works as before. To react to specific problems,
use the condition class instead of matching message text. All inherit from
`fedstat_error`; `e$stage` names the step (`metadata`, `token`, `grid`,
`export`, `parse`, `schedule`) and HTTP errors carry `e$status`.

| Class | Meaning |
| --- | --- |
| `fedstat_transient` | Temporary network or server failure; try again later. `fedstat_timeout` is a subclass for requests that took too long. |
| `fedstat_rate_limit` | HTTP 429; `e$retry_after` gives the wait in seconds when Fedstat sends it. |
| `fedstat_blocked` | HTTP 403 or a page that looks like an access check. |
| `fedstat_token`, `fedstat_redirect` | Fedstat rejected the download session. |
| `fedstat_unexpected_response` | An HTML page or unexpected file type instead of data. |
| `fedstat_metadata`, `fedstat_parse` | The indicator page or export had an unexpected structure. |
| `fedstat_truncated` | Fedstat marked the export as incomplete. |
| `fedstat_too_large` | The request exceeds the size limit; nothing was sent. |
| `fedstat_dictionary_changed` | Fedstat changed the filters during a `fedstat_download()`. |
| `fedstat_time_limit`, `fedstat_unavailable`, `fedstat_budget` | A `fedstat_download()` run stopped; saved parts are kept, run it again later. |

Pressing Esc or Ctrl+C is an R `interrupt`, not an error, and is never retried.

## Network settings

Every function accepts httr settings in `...`; they apply to every request,
including token renewal:

```r
job <- fedstat_download(
    "37089", filters = my_filters, cache_dir = "wages",
    timeout_seconds = 300,
    httr::user_agent("my-project/1.0"),
    httr::config(connecttimeout = 20)
)
```

Settings from `httr::set_config()` are respected. Package-wide defaults can be
set with `options(fedstatAPIr.config = httr::config(...))`; settings passed in
a call take precedence.
