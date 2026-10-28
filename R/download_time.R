# Monotonic elapsed time includes HTTP waits and is unaffected by clock changes.
fedstat_elapsed <- function() proc.time()[["elapsed"]]

fedstat_time_expired <- function(seconds, waiting = FALSE) {
    fedstat_error(paste0(
        if (waiting) "The next required pause exceeds the remaining time allowance" else "Download time limit reached",
        " (", seconds, " s per run)."
    ), "time_limit", "schedule")
}
