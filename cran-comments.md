## Submission

fedstatAPIr 2.0.0. A major release: new functions `fedstat_download()`,
`fedstat_collect()` and `fedstat_cleanup()` download data in small, paced,
resumable parts, and every function that requests data from Fedstat now limits
the size of one request to protect the service. `fedstat_data_load_with_filters()`
is deprecated but kept. See NEWS.md.

## Test environments
* win 11 x86_64, R 4.3.2

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE, from the local machine only:

* checking for future file timestamps ... NOTE
  unable to verify current time

## Tests
Examples that contact Fedstat are wrapped in `\dontrun{}`: the service is
slow, may be unreachable outside Russia, and should not receive automated
traffic. The offline regression tests in `tests/` use local fixtures and make
no network requests.
