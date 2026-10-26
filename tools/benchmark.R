# Offline benchmark against an earlier parser. No live service requests.
# Rscript tools/benchmark.R old-parser.R old|new
library(fedstatAPIr)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[2L]] %in% c("old", "new"))
source <- paste(readLines("tests/fixtures/generic.xml"), collapse = "\n")
series <- regmatches(source, regexpr("(?s)<generic:Series>.*?</generic:Series>", source, perl = TRUE))
# 100,000 synthetic observations, generated before timing either implementation.
start <- regexpr("<DataSet>", source, fixed = TRUE)
end <- regexpr("</DataSet>", source, fixed = TRUE)
bytes <- charToRaw(paste0(
    substr(source, 1, start + nchar("<DataSet>") - 1L),
    paste(rep(series, 50000L), collapse = "\n"), substr(source, end, nchar(source))
))
parser <- fedstat_parse_sdmx_to_table
if (args[[2L]] == "old") {
    env <- new.env(parent = asNamespace("fedstatAPIr"))
    sys.source(args[[1L]], env)
    parser <- env$fedstat_parse_sdmx_to_table
}
invisible(gc())
profile <- tempfile()
Rprofmem(profile)
timing <- system.time(data <- parser(bytes, try_to_parse_ObsValue = FALSE))
Rprofmem(NULL)
stopifnot(nrow(data) == 100000L, all(data$AREA_code == "001"))
allocations <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
unlink(profile)
cat(
    args[[2L]], "rows=", nrow(data), "input_MB=", round(length(bytes) / 1e6, 1),
    "elapsed=", timing[["elapsed"]], "R_allocated_MB=", round(sum(allocations, na.rm = TRUE) / 1e6, 1), "\n"
)
# Linux process high-water RSS includes fixture generation and package loading.
if (file.exists("/proc/self/status")) cat(grep("VmHWM", readLines("/proc/self/status"), value = TRUE), "\n")
