# Opt-in, tiny integration check; never run by R CMD check.
# FEDSTAT_LIVE=1 Rscript tools/live.R /tmp/fedstat-live
stopifnot(identical(Sys.getenv("FEDSTAT_LIVE"), "1"))
library(fedstatAPIr)
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else tempfile("fedstat-live-")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
config <- httr::config(connecttimeout = 20, low_speed_limit = 1, low_speed_time = 60)
started <- Sys.time()
ids <- fedstat_get_data_ids("37089", config, timeout_seconds = 90, retry_max_times = 1)
filters <- lapply(split(ids, ids$filter_field_title), function(x) x$filter_value_title[1L])
year <- unique(ids$filter_field_title[ids$filter_field_id == "3"])
stopifnot(length(year) == 1L, all(c("2010", "2011") %in% ids$filter_value_title[ids$filter_field_id == "3"]))
filters[[year]] <- c("2010", "2011")
primorye <- ids[ids$filter_value_title == "\u041f\u0440\u0438\u043c\u043e\u0440\u0441\u043a\u0438\u0439 \u043a\u0440\u0430\u0439", ]
stopifnot(nrow(primorye) == 1L)
filters[[primorye$filter_field_title]] <- primorye$filter_value_title
# The indicator itself is automatically selected by the public filter helper.
filters[[unique(ids$filter_field_title[ids$filter_field_id == "0"])]] <- NULL
selected <- fedstat_data_ids_filter(ids, filters)
Sys.sleep(30)
bytes <- fedstat_post_data_ids_filtered(selected, config, timeout_seconds = 90, retry_max_times = 1)
writeBin(bytes, file.path(out, "baseline.xml"))
baseline <- fedstat_parse_sdmx_to_table(bytes, try_to_parse_ObsValue = FALSE)
stopifnot(nrow(baseline) > 0L)
cat("Unsplit export:", nrow(baseline), "rows,", length(bytes), "bytes\n")
Sys.sleep(30)
reused <- tryCatch(
  {
    again <- fedstat_post_data_ids_filtered(selected, config, timeout_seconds = 90, retry_max_times = 1)
    identical(baseline, fedstat_parse_sdmx_to_table(again, try_to_parse_ObsValue = FALSE))
  },
  error = function(e) paste(class(e)[1L], conditionMessage(e))
)
cat("Same-session token reuse:", reused, "\n")
Sys.sleep(30)
job <- fedstat_download("37089", config,
  filters = filters, path = file.path(out, "parts"),
  max_combinations = 1, time_limit_seconds = 600, request_pause = 30,
  timeout_seconds = 90, retry_max_times = 2
)
combined <- fedstat_collect(job)
canonical <- function(x) {
  x <- x[sort(names(x))]
  x <- x[do.call(order, x), , drop = FALSE]
  rownames(x) <- NULL
  x
}
stopifnot(identical(canonical(baseline), canonical(combined)))
# A completed resume must not need the network.
resumed <- fedstat_download("37089", filters = filters, path = job$path)
stopifnot(resumed$complete)
cat(
  "PASS: split == unsplit; completed resume verified. Elapsed minutes:",
  round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2), "\n"
)
