# fedstatAPIr

[Русская версия](README.md)

An R package that downloads statistics from [Fedstat / EMISS](https://www.fedstat.ru),
the Russian official statistics portal: choose an indicator and filters, get a
table. Saves you ⏳ and nerves 😡. This is an unofficial client.

> **Access:** Fedstat may only open from Russia and some CIS countries. If
> www.fedstat.ru does not open in your browser, the package can't reach it
> either; you need an internet connection with a Russian address.

## Install

```r
install.packages("fedstatAPIr")
```

The latest version from GitHub:

```r
# install.packages("remotes") # if remotes is not installed yet
remotes::install_github("DenchPokepon/fedstatAPIr")
```

## Quick example

Download [consumer price indices (indicator 31074)](https://www.fedstat.ru/indicator/31074)
for January 2024 in Primorsky Krai: all goods and services, compared with the
previous month and with January of the previous year.

The indicator code is in the page address. **Write filter names and values in
Russian**, exactly as Fedstat shows them; the comments translate them. Copy the
whole example into R or RStudio and run it:

```r
library(fedstatAPIr)

filters <- list(
    "Год" = "2024", # Year
    "Период" = "Январь", # Period: January
    # Territory: Primorsky Krai
    "Классификатор объектов административно-территориального деления (ОКАТО)" = "Приморский край",
    "Виды показателя" = c( # Index type
        "К предыдущему месяцу", # vs previous month
        "К соответствующему периоду предыдущего года" # vs same period last year
    ),
    "Виды товаров и услуг" = "*" # Goods and services: "*" means all values
)

job <- fedstat_download(
    indicator_id = "31074",
    filters = filters,
    cache_dir = "cpi-download" # folder for the downloaded parts
)
data <- fedstat_collect(job) # your table
```

You get a table `data` with 1,500 rows: 750 goods and services × 2 index types.
The first rows of the main columns (real Fedstat data):

```r
head(data[c("s_grtov", "s_POK", "ObsValue")], 5)
#>                              s_grtov                                       s_POK ObsValue
#> 1                Все товары и услуги                        К предыдущему месяцу   100.72
#> 2                Все товары и услуги К соответствующему периоду предыдущего года   109.31
#> 3                         Все товары                        К предыдущему месяцу   100.87
#> 4                         Все товары К соответствующему периоду предыдущего года   109.81
#> 5 Базовый индекс потребительских цен                        К предыдущему месяцу   100.81
```

* `ObsValue` holds the values, as numbers.
* Fedstat names the other columns with its own codes: here `s_grtov` is goods and
  services, `s_POK` the index type, `s_OKATO` the territory. `Time` is the year,
  `PERIOD` the period, `EI` the unit. Columns ending in `_code` hold
  classification codes.
* The folder `cpi-download` is created in R's current folder (the project folder
  in an RStudio project). A full path such as `"G:/research/cpi-download"` works too.

About filters:

* several values go in `c()`: `"Год" = c("2023", "2024")`;
* `"*"` selects every value of a filter;
* every filter with more than one value on the website must be listed. If you
  miss one, the error names it;
* case and extra spaces don't matter.

## Why download in parts? 🧩

Fedstat builds a table from every combination of the values you choose.
20 years × 12 months × 90 regions × 1,000 products is 21.6 million combinations.
Even when most are empty, Fedstat has to check each one. Such requests often hang
or fail, and they slow the site down for everyone else.

`fedstat_download()` cuts your request into small parts, pauses between them and
saves every finished part to disk. Fedstat gets a light, steady load; you get a
download that finishes and continues where it stopped.

The example above is about 2,200 combinations: a single request. Parts matter for
bigger selections: by default a part has up to 100,000 combinations, with a
30–60 second pause between requests.

## If the download stops 🔁

Run the same code again, with the same filters and the same `cache_dir`. Finished
parts are skipped and only the rest is downloaded. This works after restarting R
or your computer, too.

Each run has one hour, including pauses. To allow two hours, add
`total_time_limit_seconds = 7200` to the `fedstat_download()` call.

The percentage in the messages counts processed combinations, not time remaining.

You can reopen a finished download later, even offline:

```r
data <- fedstat_collect("cpi-download")
```

One folder per selection: use another `cache_dir` for other filters.

## What you see → what to do

Find the start of the message in the table.

| Message | What happened | What to do |
| --- | --- | --- |
| `Download time limit reached` | The run used its hour | Run the same code again |
| `Automatic recovery stopped` | Fedstat isn't responding or keeps returning errors | Wait an hour or two, then run the same code again |
| `Fedstat may be checking or restricting access`, `Fedstat refused access` | Fedstat is limiting access | The package waits by itself. If the download stopped, check that www.fedstat.ru opens in your browser and try later |
| `This download folder is in use by another R session` | Another R session is using the folder | Wait for it. If R crashed, delete the `.lock` folder; the message gives its path |
| `Fedstat changed the filters`, `Fedstat returned data for values that were not requested` | Fedstat changed the indicator's filters | Start again with a new `cache_dir` |
| `Fedstat renamed some values`, `Fedstat no longer offers some of the values you selected` | Fedstat renamed or removed some values | Nothing: the download continues. `print(job)` lists removed values |
| `Some ObsValue values are not numbers` | The data contain notes instead of numbers | `fedstat_collect(job, try_to_parse_ObsValue = FALSE)` returns the values as text |
| `Unknown argument` | A setting name is misspelled | Check the spelling in `?fedstat_download` |

**Parts keep failing?** Add `preview_first = TRUE` to the call. Before each part,
the package then asks Fedstat to show the table first, like pressing "Apply" on
the website, and only then downloads it. Sometimes Fedstat only returns data this
way. The download takes about twice as long. You can switch it on in the middle
of a download, in the same folder.

## Big downloads

* `filters = "*"` instead of a list selects everything: all years, territories
  and other values. This can take many hours.
* `max_nominal_cells_request` sets the part size in combinations. 100,000 is
  reliable; you can try up to 250,000, above that the package warns you.
* `timeout_seconds = 300` waits up to 5 minutes for a response instead of 3.
* `fedstat_collect()` loads the whole table into memory.

All settings and details: `?fedstat_download`.

## Free up space

Parts are not deleted automatically. Save your table, then remove them:

```r
saveRDS(data, "cpi-2024.rds") # save the table
fedstat_cleanup(job)          # delete the downloaded parts
```

Open the saved table with `data <- readRDS("cpi-2024.rds")`.

## Find an indicator

The indicator code is in the address of its page on fedstat.ru. The package also
includes a list of indicators (as of its release); the code is in the `id` column:

```r
ind <- fedstat_indicators_names_database
ind[grepl("потребительских цен", ind$title), c("id", "title")] # "consumer prices"
```

`fedstat_indicator_info()` downloads the current list.

## For advanced users

`fedstat_get_data_ids()`, `fedstat_data_ids_filter()`,
`fedstat_post_data_ids_filtered()` and `fedstat_parse_sdmx_to_table()` are the
separate steps of a single request, without parts or pauses.
`fedstat_data_load_with_filters()` from version 1.x still works but is
deprecated. See [MIGRATION.md](MIGRATION.md) for what changed since 1.x.

## Help 💬

Help pages: `?fedstat_download`, `?fedstat_collect`, `?fedstat_cleanup`.

If something doesn't work, open a
[GitHub issue](https://github.com/DenchPokepon/fedstatAPIr/issues) and attach
`download.log` from the download folder. You don't need to read it; it contains
no passwords, cookies or tokens. You can also email deniskrylovvit@gmail.com.

## Support development

If the package helps your work, you can [support its development](https://pay.cloudtips.ru/p/db468def).
Thank you for using it!

## Contribute

Run `R CMD check --as-cran` before committing changes. Formatting and validation
commands are in [tools/README.md](tools/README.md).
