# fedstatAPIr

[Русская версия](README.md)

Download [Fedstat / EMISS](https://www.fedstat.ru) statistics into R. Choose an
indicator and filters; the package splits large requests, saves progress and lets
you continue after a failure. This is an unofficial client.

## Install

Install the released version from [CRAN](https://CRAN.R-project.org/package=fedstatAPIr):

```r
install.packages("fedstatAPIr")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("DenchPokepon/fedstatAPIr")
```

The examples below use `fedstat_download()` and require development version
1.1.0.9007 or later.
To install a downloaded source copy, run `R CMD INSTALL .` in its folder.

## Download data

This example selects CPI for January 2024 in Primorsky Krai, for all goods and
services. **31074** is the indicator code at the end of its
[page address](https://www.fedstat.ru/indicator/31074). Use the Russian filter
labels shown on that page.

```r
library(fedstatAPIr)

job <- fedstat_download(
  indicator_id = "31074",       # Consumer price indices
  path = "cpi-primorye-2024",    # Data folder; created automatically
  filters = list(
    "Год" = "2024",             # Year
    "Период" = "Январь",        # January
    "Классификатор объектов административно-территориального деления (ОКАТО)" = "Приморский край",
    "Виды показателя" = c(
      "К предыдущему месяцу",   # Compared with the previous month
      "К соответствующему периоду предыдущего года" # Compared with the same period last year
    ),
    "Виды товаров и услуг" = "*" # All goods and services
  )
)

data <- fedstat_collect(job)     # Combine the saved parts into one table
head(data)
```

Use `c("2023", "2024")` to select two years. `"*"` selects all values of a field.
Every field with multiple possible values must be specified; missing fields are
listed in the error. Case and extra whitespace are ignored.

Inspect filter labels in R with:

```r
filters_table <- fedstat_get_data_ids("31074")
View(filters_table[c("filter_field_title", "filter_value_title")])
```

`ObsValue` retains source text, including decimal commas and markers such as `...`.
Columns ending in `_code` retain classification codes.

## Resume a download

Repeat the same `fedstat_download()` call with the same filters and folder.
Verified completed parts are skipped, including after restarting R. Once finished:

```r
data <- fedstat_collect("cpi-primorye-2024")
```

The folder contains `download.log`, `manifest.rds` (selection and progress), and
`parts/` (saved tables). These files need a persistent location to support resume;
they do not belong in system temporary storage. Older flat folders are supported.
Use a different folder for a different selection. `fedstat_collect()` loads the
entire result into memory; larger jobs can read individual parts with `readRDS()`.

## Why split requests?

Filters combine: 20 years × 12 months × 90 territories × 1,000 products gives
21.6 million possible combinations. Actual observations may be much fewer, but the
request can still be expensive. Output row count is not known ahead of time.

The package halves the field with the most selected values until each part fits
`max_nominal_cells_request`; the other fields retain their selection. After two
failed attempts (the default), transient data failures may cause another split.
Ambiguous redirects are only split after a token refresh has also failed to help.
Metadata/token GET failures, explicit token errors, rate limits and access refusals
do not trigger splitting. A split roughly halves combinations, not necessarily
bytes or response time. By default it
waits 30–60 seconds between requests to reduce load on the shared service.
Temporary failures are retried with pauses; persistent failures stop the download
without discarding saved data. `filters = "*"` selects the indicator's entire
history across all dimensions and can take a long time.

Each invocation has a **one-hour time allowance by default**, including requests,
pauses and retries. Saved progress is retained when time runs out. Local parsing,
validation and saving finish safely and may extend the elapsed time. Set
`time_limit_seconds = 7200` to allow two hours. Resuming starts a fresh allowance.

## Settings

Usually the indicator, filters and folder are enough. Optional arguments include:

| Argument | Meaning |
| --- | --- |
| `max_nominal_cells_request = 100000` | Up to 100,000 nominal cells per request: the default for smaller, usually more reliable requests. |
| `time_limit_seconds = 3600` | One hour per invocation, including pauses and retries. Default: 3,600 seconds. |
| `timeout_seconds = 300` | Allow five minutes per request, including transfer. |
| `request_pause = c(30, 60)` | Wait 30–60 seconds between requests. |

The size is the **Cartesian product** of the selected filter values: years ×
months × territories × products × other dimensions. For example, 2 × 12 × 10 ×
100 = 24,000 nominal cells. Actual output can have fewer rows because some
combinations have no observations.

Start with **100,000** for reliability, accepting that it may take longer.
The best value depends on the indicator and Fedstat's current health. **250,000**
is not a guaranteed safe threshold; larger settings produce a warning.
Existing partitions are not merged on resume.

```r
max_nominal_cells_request = 250000 # Up to 250,000 filter combinations, not output rows
```

Short console messages show saved rows and the percentage of selected filter
combinations covered by saved parts. This survives further splitting and resume.
It is **not a time estimate**: combinations vary in speed and may have no data.
At 100%, final integrity checks still need to finish. `verbose = FALSE`
hides updates; warnings and errors remain visible. Use `verbose = "detailed"`
for technical events. The full `download.log` is written in every mode. Messages
also work in the RStudio console; interrupting a call retains saved progress.

A suspected access check or refusal produces a warning and a five-minute pause
before refreshing the download session. Repeated refusals increase the pause;
recovery remains bounded by the time allowance and retry settings. This does not
solve CAPTCHAs, and the detection alone does not prove a CAPTCHA was presented.
See `?fedstat_download` for `access_pause` and other network/retry settings.

## Prepare the website preview first

If direct exports fail, try adding `prepare_grid = TRUE` to the same download
call. Before **each export attempt**, this applies that part's filters to
`dataGrid.do` using the same session, like the website's Apply button. Preview
results are discarded; only validated SDMX is saved. An empty preview is not
accepted as evidence of a complete empty SDMX export.

The default is `FALSE`: this adds a request and server work for each attempt.
Preview requests share pacing, HTTP configuration and the one-hour allowance.
This may help some failures but does not guarantee faster downloads or bypass
access checks. You can enable it when resuming an existing folder. The diagnostic
`tools/prices.R` script enables it to exercise this path.

If the preview explicitly reports zero observations but SDMX redirects (HTTP 302),
the package refreshes the session and retries. If that still fails, it explains
the conflict and retains the unfinished part instead of repeatedly subdividing
it. Other saved parts remain safe. An empty preview alone does not mark a part
complete.

## Removing saved parts

Save the collected table first, then remove the parts:

```r
data <- fedstat_collect(job)
saveRDS(data, "cpi-2024.rds")
fedstat_cleanup(job)
```

Alternatively, `data <- fedstat_collect(job, remove_parts = TRUE)` removes parts
only after the entire table has been read and validated. The table then exists
**only in R memory** until you save it. Logs, the manifest and unrelated files
remain. A cleaned download cannot be resumed or collected again.

## Stopping on Windows

Ctrl+C retains saved parts. In R/RStudio it returns control to the session;
network calls may take time to observe cancellation. The Windows R 4.3
`bin/Rscript.exe` wrapper can return to CMD before its child exits, leaving a late
cancellation message after the prompt. Run `bin/x64/Rscript.exe` directly instead
(adjust your installed version):

```bat
"C:\Program Files\R\R-4.3.2\bin\x64\Rscript.exe" tools/prices.R ../fedstat-price-check
```

Wait for the process to finish before restarting. After a forced termination,
remove a stale `.lock` directory only once no R process is still using the job.

## Troubleshooting

Attach `download.log`, the package version and `sessionInfo()` to a
[GitHub issue](https://github.com/DenchPokepon/fedstatAPIr/issues). Logs include
request stages, warnings and stop reasons; tokens, cookies and response bodies
are excluded.

HTTP 302 does not distinguish a session failure from a selection problem. The
package tries token refresh and smaller requests, but never treats a redirect as
an empty table. A persistently rejected part remains incomplete.

Existing functions, including `fedstat_data_load_with_filters()`, remain available.
Individual metadata, filtering, export and SDMX parsing steps have examples in
R help. A bundled indicator catalogue is in `fedstat_indicators_names_database`.

## Support and development

Contact: deniskrylovvit@gmail.com.
[Support development](https://pay.cloudtips.ru/p/db468def).

Run `R CMD check --as-cran` before committing. Formatting, checks and live-test
commands are in [tools/README.md](tools/README.md).
