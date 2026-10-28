# Bounded CPI integration workload, deliberately separate from offline tests.
# Rscript tools/prices.R [output-directory] [year] [period-label]
library(fedstatAPIr)
local({
    previous <- options(warn = 1) # Print warnings immediately, restoring options on exit.
    on.exit(options(previous), add = TRUE)
    args <- commandArgs(trailingOnly = TRUE)
    path <- if (length(args)) args[[1L]] else "fedstat-price-check"
    year <- if (length(args) >= 2L) args[[2L]] else "2024"
    month <- if (length(args) >= 3L) args[[3L]] else "\u044f\u043d\u0432\u0430\u0440\u044c"
    if (packageVersion("fedstatAPIr") < "2.0.0") stop("Install fedstatAPIr 2.0.0 or later first.")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path <- normalizePath(path, mustWork = TRUE)
    # No tokens, cookies or HTTP headers are included in the diagnostic files.
    cat(
        paste(c(
            paste("Captured:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
            capture.output(sessionInfo()), ""
        ), collapse = "\n"),
        file = file.path(path, "session-info.txt"), append = TRUE
    )
    filters <- list(
        "\u0413\u043e\u0434" = year,
        "\u041f\u0435\u0440\u0438\u043e\u0434" = month,
        "\u0412\u0438\u0434\u044b \u043f\u043e\u043a\u0430\u0437\u0430\u0442\u0435\u043b\u044f" = "*",
        "\u0412\u0438\u0434\u044b \u0442\u043e\u0432\u0430\u0440\u043e\u0432 \u0438 \u0443\u0441\u043b\u0443\u0433" = "*",
        "\u041a\u043b\u0430\u0441\u0441\u0438\u0444\u0438\u043a\u0430\u0442\u043e\u0440 \u043e\u0431\u044a\u0435\u043a\u0442\u043e\u0432 \u0430\u0434\u043c\u0438\u043d\u0438\u0441\u0442\u0440\u0430\u0442\u0438\u0432\u043d\u043e-\u0442\u0435\u0440\u0440\u0438\u0442\u043e\u0440\u0438\u0430\u043b\u044c\u043d\u043e\u0433\u043e \u0434\u0435\u043b\u0435\u043d\u0438\u044f (\u041e\u041a\u0410\u0422\u041e)" = "*"
    )
    cat("CPI 31074:", year, month, "- all territories, goods and indicator types.\n")
    cat("Diagnostic log:", file.path(path, "download.log"), "\n")
    job <- tryCatch(fedstat_download(
        "31074",
        filters = filters, cache_dir = path,
        max_nominal_cells_request = 100000, token_policy = "reuse",
        preview_first = TRUE, # Apply each part's filters to the website preview before export.
        total_time_limit_seconds = 3600, retry_max_times = 2L, max_failures = 6L,
        request_pause = c(30, 60), timeout_seconds = 300,
        httr::user_agent("fedstatAPIr-price-check/1.0"),
        httr::config(connecttimeout = 20, low_speed_limit = 1, low_speed_time = 90)
    ), interrupt = function(e) {
        if (interactive()) invokeRestart("abort")
        quit(save = "no", status = 130, runLast = FALSE)
    })
    print(job)
    cat("The same command resumes this job. Send download.log and session-info.txt for diagnosis.\n")
})
