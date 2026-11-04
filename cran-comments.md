## Submission

fedstatAPIr 2.1.0, a major update from 1.1.0, the current CRAN version.
New functions `fedstat_download()`, `fedstat_collect()` and
`fedstat_cleanup()` download data in small, paced, resumable parts;
`fedstat_show_filters()` and `fedstat_dictionary()` describe an indicator's
filters and codes. Every function that requests data now limits the size of
one request to protect the service. `fedstat_data_load_with_filters()` is
deprecated but kept. See NEWS.md.

## Test environments

* Ubuntu 26.04 x86_64, R 4.6.2 (GitHub Actions): 0 errors, 0 warnings, 0 notes
* Windows Server 2022 x64, R 4.6.2 (GitHub Actions): 0 errors, 0 warnings, 0 notes
* Windows 11 x86_64, R 4.3.2 (local)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE on the local machine only:

* checking for future file timestamps ... NOTE
  unable to verify current time

This comes from the local network and does not concern the package.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Tests

Examples that contact Fedstat are wrapped in `\dontrun{}`: the service is
slow, may be unreachable outside Russia, and should not receive automated
traffic. The offline regression tests in `tests/` use local fixtures and make
no network requests.
