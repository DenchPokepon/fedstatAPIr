# Development checks

Offline regression tests run during `R CMD check`. They use base R assertions and
small fixtures, without a test framework or live Fedstat traffic.

```sh
R CMD build .
R CMD check --as-cran fedstatAPIr_1.1.0.9002.tar.gz
```

For a quicker iteration after installing the source:

```sh
R CMD INSTALL .
Rscript tests/regression.R
```

Coverage includes metadata/session separation, HTTP configuration, typed errors,
SDMX parsing, duplicate keys, bounded retries, token GET recovery, post-refresh
redirect subdivision, cooldown recovery, Retry-After, request budgets,
interruptions, log contents, checkpoint integrity and old flat-folder resume.
Warnings are logged but retain normal R warning semantics.

## Tiny live check

Opt-in only; do not add this to routine CI:

```sh
FEDSTAT_LIVE=1 Rscript tools/live.R ../fedstat-live-check
```

In Windows Command Prompt, first run `set FEDSTAT_LIVE=1`, then
`Rscript tools/live.R ../fedstat-live-check`.

This requests indicator 37089 for Primorsky Krai in 2010 and 2011. It compares an
unsplit export with two one-cell parts, tests token reuse and checks that completed
resume needs no network. Requests are sequential with 30-second pauses and a
bounded request count. The output folder contains the raw baseline and saved parts.
A pass describes that indicator/selection at that time, not the whole API.

## Price workload and diagnostic files

```sh
R CMD INSTALL .
Rscript tools/prices.R ../fedstat-price-check
```

Defaults: indicator 31074, January 2024, all territories, goods and indicator
types. Optional positional arguments are output directory, year and period label.
This is one month, not the entire historical dataset. The script requires version
1.1.0.9002 or later, prints warnings immediately and saves session information.

The limit is 40 HTTP requests per invocation, including metadata, token GETs,
retries and recovery. Requests have 30–60 second pauses, a 300-second total timeout,
20-second connection timeout and 90-second low-speed limit. The downloader can
recover automatically after a cooldown and try smaller parts following repeated
export rejection. It never resets the request budget automatically. Service
outages or persistent refusals may still prevent completion; the same command
resumes saved progress. Do not change the selection while reusing its folder.

Send `download.log` and `session-info.txt` from the output folder for diagnosis.
Both append across runs. `download.log` is UTF-8 JSON Lines, with run IDs, UTC
timestamps, request counts, saved/pending parts, HTTP stages, response sizes/times,
waits, token refreshes, split reasons, warnings and recovery/stop summaries. Headers,
cookies, token values and response bodies are excluded. For checkpoint issues,
`manifest.rds` may help; data shards are not needed initially.

```r
log <- jsonlite::stream_in(file("../fedstat-price-check/download.log"))
```

## Parser benchmark

```sh
Rscript tools/benchmark.R
```

The benchmark generates synthetic SDMX without network requests. Compare elapsed
time and allocations on the same machine/R version. Lower allocation does not
necessarily mean faster parsing. The real trimmed fixture
`tests/fixtures/fedstat-37089.xml` covers Fedstat SDMX 1.0; the synthetic generic
fixture covers namespaces, raw markers and validation cases.

## README maintenance

`README.Rmd` is the Russian source; its examples are plain fenced code, so rendering
does not contact Fedstat. `README.md` contains the same body without the YAML header.
`README.en.md` is the separate English guide. Keep the selection and defaults in
both guides consistent. Technical migration notes remain in `MIGRATION.md`.
