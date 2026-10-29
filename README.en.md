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
  Without `cache_dir`, the package creates a folder such as `fedstat-31074-1a2b3c4d`
  and tells you where it is. The same filters always get the same folder.

About filters:

* several values go in `c()`: `"Год" = c("2023", "2024")`;
* `"*"` selects every value of a filter;
* every filter with more than one value on the website must be listed. If you
  miss one, the error names it;
* case and extra spaces don't matter.

## Which filters does an indicator have? 🔎

You can copy filter names and values from the indicator page on fedstat.ru, in
the table settings. Or ask the package:

```r
filters <- fedstat_show_filters("31074")
```

It lists every filter with its number of values and some examples, and prints a
ready `filters` list: the selection the website shows when the page opens.

```r
#> Indicator 31074: Индексы потребительских цен на товары и услуги
#>
#> Filters and their values (use them in Russian, as written here):
#>   "Год": 25 values: "2002", "2003", "2004", ..., "2026"
#>   "Период": 16 values: "январь", "февраль", "март", ..., "IV квартал"
#>   "Классификатор объектов административно-территориального деления (ОКАТО)": 119 values: "Российс...
#>   "Виды показателя": 9 values: "К декабрю предыдущего года", "К предыдущему кварталу", "К предыду...
#>   "Виды товаров и услуг": 1,105 values: "Все товары и услуги", "Все товары", "Базовый индекс потр...
#>   "Единица измерения": one value, selected automatically: "процент"
#>
#> # The selection the website shows first (768 filter combinations). "*" selects every value.
#> filters <- list(
#>     "Год" = "2024",
#>     "Период" = "*",
#>     "Классификатор объектов административно-территориального деления (ОКАТО)" = c(
#>         "Центральный федеральный округ",
#>         "Белгородская область",
#>         "Брянская область"
#>     ),
#> ...
```

This list is a starting point. Copy it into your script and replace the values
with the ones you need, for example `"Год" = c("2023", "2024")`, or `"*"` for all
values. The function also returns the list, so `fedstat_download("31074", filters)`
downloads it unchanged.

All values of one filter:

```r
ids <- fedstat_get_data_ids("31074")
ids$filter_value_title[ids$filter_field_title == "Виды товаров и услуг"] # goods and services
```

## Why download in parts? 🧩

Fedstat builds a table from every combination of the values you choose.
20 years × 12 months × 90 regions × 1,000 products is 21.6 million combinations.
Even when most are empty, Fedstat has to check each one. Such requests often hang
or fail, and they slow the site down for everyone else.

`fedstat_download()` cuts your request into small parts, pauses between them and
saves every finished part to disk. Fedstat gets a light, steady load; you get a
download that finishes and continues where it stopped.

The example above is about 2,200 combinations: a single request. Parts matter for
bigger selections: by default a part has up to 50,000 combinations, with a
30–60 second pause between requests.

No function in the package sends a request above 100,000 combinations unless you
explicitly allow it, and none sends one above 250,000 at all. Such requests almost
never succeed and slow the site down for everyone. `fedstat_download()` gets the
same selection in parts.

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
| `This request has ... filter combinations` | The request is too large for one call to Fedstat | Download it with `fedstat_download()`, which splits it into parts |
| `Fedstat returned no data` | The selection has no data (from `fedstat_dictionary()`) | Pass filters that show data on the website |
| `Unknown argument` | A setting name is misspelled | Check the spelling in `?fedstat_download` |

**Parts keep failing?** Add `preview_first = TRUE` to the call. Before each part,
the package then asks Fedstat to show the table first, like pressing "Apply" on
the website, and only then downloads it. Sometimes Fedstat only returns data this
way. The download takes about twice as long. You can switch it on in the middle
of a download, in the same folder.

## Big downloads

* `filters = "*"` instead of a list selects everything: all years, territories
  and other values. This can take many hours.
* `max_nominal_cells_request` sets the part size in combinations. The default,
  50,000, is reliable for any indicator. If an indicator has many empty
  combinations (for example, not every product is sold in every region every
  month), parts are mostly empty and Fedstat answers them quickly. Then larger
  parts make the download much faster and are still reliable: try
  `max_nominal_cells_request = 100000`. Above 50,000 the package warns you, above
  100,000 it also needs `allow_large_requests = TRUE` (which also turns off the
  warning), and 250,000 is the maximum.
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

## Code dictionary 📖

The values of many filters (territory, product, economic activity and so on)
come from classifications. For each such filter the data table has two columns:
one with names and one with codes. Fedstat names these columns, and they differ
between indicators: in the consumer price example above, goods and services are
`s_grtov` and `s_grtov_code`.

`fedstat_dictionary()` downloads the full dictionary of an indicator: every code
and name of every classification, including values that are not in your
download. First see which classifications it has:

```r
dictionary <- fedstat_dictionary("31074")
unique(dictionary[c("field_id", "field_title")])
#>      field_id                                                             field_title
#> 1       s_POK                                                         Виды показателя
#> 10    s_grtov                                                    Виды товаров и услуг
#> 1115  s_OKATO Классификатор объектов административно-территориального деления (ОКАТО)
```

`field_id` is the column name in the data. Codes and names of one classification:

```r
dictionary[dictionary$field_id == "s_grtov", c("value_id", "value_title")]
#>    value_id                        value_title
#> 10        1                Все товары и услуги
#> 11        2                         Все товары
#> 12        3 Базовый индекс потребительских цен
#> ...
```

This dictionary has 1,105 goods and services, while the example above has 750:
Primorsky Krai did not have data for all of them in January 2024. The dictionary
comes from the SDMX export, not from the indicator page, so its codes are the
ones in the `_code` columns.

Fedstat sends the dictionary only together with data, so the function downloads
a small piece of data. It starts with the selection the website shows when the
page opens and, if that has no data, tries a few small random selections. If
that fails too (`Fedstat returned no data`), pass filters that show data on the
website: `fedstat_dictionary("31074", filters = filters)`.

Year, period and unit of measurement are not in the dictionary: they have no
codes.

## For advanced users

`fedstat_get_data_ids()`, `fedstat_data_ids_filter()`,
`fedstat_post_data_ids_filtered()` and `fedstat_parse_sdmx_to_table()` are the
separate steps of a single request, without parts or pauses.
`fedstat_data_load_with_filters()` from version 1.x still works but is
deprecated. The same request size limits apply to them.

Each function has a detailed help page with examples: `?fedstat_get_data_ids`,
`?fedstat_data_ids_filter`, `?fedstat_post_data_ids_filtered`,
`?fedstat_parse_sdmx_to_table`. For example, `fedstat_post_data_ids_filtered()`
can also download the table as Excel (`data_format = "excel"`). See
[MIGRATION.md](MIGRATION.md) for what changed since 1.x.

## If Fedstat refuses the package 🚪

If the package is refused (`Fedstat refused access`) while the website opens in
your browser, Fedstat may be filtering programs by their User-Agent header. The
package introduces itself as `fedstatAPIr/<version>`. Set your browser's
User-Agent once per R session, before downloading:

```r
httr::set_config(httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
))
```

A page such as "what is my user agent" shows your browser's value. For a single
call, add `httr::user_agent("...")` to `fedstat_download()` instead. Other network
settings are passed the same way, for example a proxy:
`httr::use_proxy("http://proxy.example:8080")`.

## Help 💬

Help pages: `?fedstat_download`, `?fedstat_collect`, `?fedstat_show_filters`,
`?fedstat_dictionary`, `?fedstat_cleanup`.

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
