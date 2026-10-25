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
1.1.0.9004 or later.
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

The package limits combinations per part and halves large requests. By default it
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
| `max_combinations = 500000` | Up to 500,000 filter combinations per initial part, not output rows. Default: 100,000. |
| `time_limit_seconds = 3600` | One hour per invocation, including pauses and retries. Default: 3,600 seconds. |
| `timeout_seconds = 300` | Allow five minutes per request, including transfer. |
| `request_pause = c(30, 60)` | Wait 30–60 seconds between requests. |

Increasing `max_combinations` usually reduces the number of parts in a new job,
but makes each request heavier for Fedstat. Existing partitions are not merged on resume. Applied
settings are shown at startup. See `?fedstat_download` and [MIGRATION.md](MIGRATION.md)
for retry, token, User-Agent and connection settings.

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
