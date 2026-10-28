# fedstatAPIr

[Русская версия](README.md)

Download [Fedstat / EMISS](https://www.fedstat.ru) statistics into R: choose an
indicator, years and territories, then collect a table for your research.
This is an unofficial client.

> **Access from outside Russia:** use a VPN with a server in Russia to access
> Fedstat. Connections from some CIS countries may also work. If you already
> have a Russian IP address, a VPN is not needed.

## Install

From CRAN:

```r
install.packages("fedstatAPIr")
```

Or from GitHub:

```r
# install.packages("remotes") # If remotes is not installed yet
remotes::install_github("DenchPokepon/fedstatAPIr")
```

## Why download in parts?

Fedstat can be slow or interrupt a download. Filters also multiply: 20 years ×
12 months × 90 territories × 1,000 products means 21.6 million possible
combinations. Many may have no observations, but the request can still be costly.

`fedstat_download()` requests smaller parts, pauses between requests and retries
failures. It saves completed parts so an interruption does not mean starting over.
This makes downloads easier to finish and reduces load on a shared service.

## Download data

Get [consumer price indices, indicator 31074](https://www.fedstat.ru/indicator/31074),
for January 2024 in Primorsky Krai: all goods and services, compared with the
previous month and with the same period of the previous year.

Take the indicator code from its page address. **Keep filter names and values
in Russian**, as shown on Fedstat; the English comments below explain them.
Copy the whole example into R or RStudio and run it:

```r
library(fedstatAPIr)

# Choose the data
filters <- list(
    "Год" = "2024", # Year
    "Период" = "Январь", # January
    # Territory: Primorsky Krai
    "Классификатор объектов административно-территориального деления (ОКАТО)" = "Приморский край",
    "Виды показателя" = c(
        "К предыдущему месяцу", # Compared with the previous month
        "К соответствующему периоду предыдущего года" # Same period last year
    ),
    "Виды товаров и услуг" = "*" # All goods and services
)

# Download: https://www.fedstat.ru/indicator/31074
job <- fedstat_download(
    indicator_id = "31074", # Consumer price indices
    filters = filters,
    cache_dir = "cpi-download" # Working folder for parts; keeps progress after a failure
)

data <- fedstat_collect(job) # Combine the parts into your table
View(data)                   # Open the table in RStudio
```

Your table for analysis is `data`. The working folder `cpi-download` is created
automatically in R's current folder (usually the project folder in an RStudio
project). You can instead use a full path such as `"G:/research/cpi-download"`.

Use `c("2023", "2024")` for two years. `"*"` selects all values of a field.
If you omit a field with multiple values, the package names it in the error.
Case and extra whitespace are ignored.

The indicator values are in `ObsValue`. They are stored as text to preserve
decimal commas and source markers such as `...`.
Columns ending in `_code` contain classification codes.

## Resume a download

**Each run has one hour by default**, including pauses and retries. If time runs
out or the connection fails, repeat the call with the same filters and `cache_dir`,
then run `fedstat_collect(job)`. After restarting R, run the whole example again
to restore the filters; already downloaded parts will be skipped.

Each run gets a fresh hour. Validating and saving an already received part may
take a little extra time before stopping. See below for how to allow more time.

If Fedstat changes the selected filters during a download, the package stops
and keeps the saved parts. Start a new download in another working folder
to avoid mixing different definitions.

Keep the working folder until the download finishes. Use a different folder for
a different selection. You can reopen a completed download later:

```r
data <- fedstat_collect("cpi-download")
```

The percentage measures completed filter combinations, not time remaining.
Different parts contain different amounts of data and take different times.

## If exports keep failing

Try the same download with preparation enabled, keeping the filters and folder:

```r
job <- fedstat_download(
    indicator_id = "31074",
    filters = filters,
    cache_dir = "cpi-download",
    prepare_before_download = TRUE # The package applies the filters before downloading
)
data <- fedstat_collect(job)
```

Sometimes Fedstat only returns an export after a preliminary table request,
like pressing Apply on its website. This option follows that sequence before
each export attempt. Fedstat's behavior changes: it may help at some times and
make no difference at others.

The default is `FALSE`. Enabling it adds a second data request per attempt and
may roughly double download time, including pauses; actual timing depends on
the server. You can enable it while resuming the same folder.

The package checks parts with no observations and continues without adding rows.

## Request size and waiting

Usually the indicator, filters and working folder are enough. To allow two hours
instead of one, add `total_time_limit_seconds = 7200` inside the
`fedstat_download()` call. Other settings work the same way:

| Argument | Meaning |
| --- | --- |
| `max_nominal_cells_request = 100000` | Up to 100,000 filter combinations per request. Smaller requests are usually more reliable, but the whole download may take longer. |
| `total_time_limit_seconds = 3600` | Overall allowance for one run; one hour by default. |
| `timeout_seconds = 300` | Allow five minutes for one request instead of the default three. |
| `request_pause = c(30, 60)` | Wait 30–60 seconds between requests by default. |

The size is the **Cartesian product** of selected years × months × territories ×
products × other fields. For example, 2 × 12 × 10 × 100 = 24,000 nominal cells,
though there may be fewer actual rows. Larger parts are subdivided.

Start with **100,000** for reliability. You can try **250,000**, but the best size
depends on the indicator and Fedstat's current health. Larger values produce a
warning.

`filters = "*"` instead of a list selects all years, territories and other values
of the indicator. This can take a long time.

## Remove downloaded parts

Save the finished table, then free the space used by its parts:

```r
saveRDS(data, "cpi-2024.rds")
fedstat_cleanup(job)
```

Parts are not deleted automatically. Reopen your saved table in another session
with `data <- readRDS("cpi-2024.rds")`.

## Help

See `?fedstat_download` for settings and `?fedstat_collect` for collection.
Collection loads the whole table into memory; large datasets and lower-level
functions are covered in the [technical notes](MIGRATION.md).

`verbose = FALSE` hides progress; `verbose = "detailed"` shows diagnostic events.
If something fails, attach `download.log` from the working folder to a
[GitHub issue](https://github.com/DenchPokepon/fedstatAPIr/issues).
You do not need to interpret the log yourself. You can also email
deniskrylovvit@gmail.com.

## Support development

If the package helps your work, you can [support its development](https://pay.cloudtips.ru/p/db468def).
Thank you for using it!

## Contribute

Run `R CMD check --as-cran` before committing changes. Formatting and validation
commands are in [tools/README.md](tools/README.md).
