#!/usr/bin/env Rscript
# mkplots.R -- Main plotting entry point for mkexp2 experiments.
#
# Expected to run inside a Docker container where:
#   /work   = this plots/ submodule directory (working dir; R sources live here)
#   /data   = experiment's results/ directory (read-only CSV files)
#   /output = experiment directory (plots.pdf written here)
#
# Usage:
#   Rscript /work/mkplots.R [--performance-profile] [--speedup] [--running-time]
#                           [--output <path>] <algo1> [<algo2> ...]
#
# When no plot flags are given all three plots are generated (same as specifying
# all three flags).

options(tidyverse.quiet = TRUE)
options(warn = 1)

# ── Parse arguments ────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

do_performance_profile <- FALSE
do_speedup             <- FALSE
do_running_time        <- FALSE
explicit_plots         <- FALSE
algorithms             <- character(0)
output_file            <- "/output/plots.pdf"

i <- 1L
while (i <= length(args)) {
  arg <- args[[i]]
  if        (arg == "--performance-profile") {
    do_performance_profile <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--speedup") {
    do_speedup             <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--running-time") {
    do_running_time        <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--output") {
    i <- i + 1L
    output_file <- args[[i]]
  } else if (startsWith(arg, "--output=")) {
    output_file <- substring(arg, 10L)
  } else if (!startsWith(arg, "--")) {
    algorithms <- c(algorithms, arg)
  } else {
    cli::cli_alert_warning("Unknown argument ignored: {arg}")
  }
  i <- i + 1L
}

if (!explicit_plots) {
  do_performance_profile <- TRUE
  do_speedup             <- TRUE
  do_running_time        <- TRUE
}

if (length(algorithms) == 0L) {
  cli::cli_abort(c(
    "No algorithms specified.",
    "i" = "Usage: mkplots.R [--performance-profile] [--speedup] [--running-time] <algo1> [<algo2> ...]"
  ))
}

cli::cli_alert_info("Algorithms : {.val {algorithms}}")
cli::cli_alert_info("Output     : {.path {output_file}}")

# ── Source plot library ────────────────────────────────────────────────────────
# Working directory inside Docker is /work (the plots submodule).

source("common.R")
source("performance_profile_plot.R")
source("speedup_plot.R")
source("running_time_box_plot.R")
source("running_time_by_core_box_plot.R")

# Use plain-text labels rather than LaTeX commands for PDF output.
TEX_LABEL_TIMEOUT    <- "T"
TEX_LABEL_IMBALANCED <- "I"
TEX_LABEL_FAILED     <- "F"

# ── Colour palette ─────────────────────────────────────────────────────────────

n_colors <- max(length(algorithms), 3L)
palette  <- RColorBrewer::brewer.pal(n_colors, "Set1")[seq_along(algorithms)]
colors   <- stats::setNames(palette, algorithms)

# ── Load datasets ──────────────────────────────────────────────────────────────

cli::cli_h2("Loading datasets")

dfs <- lapply(algorithms, function(algo) {
  cli::cli_alert_info("Loading {.val {algo}}")
  tryCatch(
    load_dataset(algo, algo, cache = FALSE),
    error = function(e) {
      cli::cli_alert_danger("Failed to load {.val {algo}}: {e$message}")
      NULL
    }
  )
})
names(dfs) <- algorithms

failed_loads <- vapply(dfs, is.null, logical(1L))
if (any(failed_loads)) {
  cli::cli_alert_warning(
    "Dropping algorithms with load errors: {.val {algorithms[failed_loads]}}"
  )
  dfs        <- dfs[!failed_loads]
  algorithms <- algorithms[!failed_loads]
  colors     <- colors[!failed_loads]
}

if (length(dfs) == 0L) {
  cli::cli_abort("No datasets loaded successfully.")
}

# ── Find common (Graph, K) instances across all datasets ──────────────────────

if (length(dfs) >= 2L) {
  common_key <- Reduce(common_rows, dfs)
  dfs_common <- lapply(dfs, function(df) df %>% common_rows(common_key))
  cli::cli_alert_info("Common instances: {nrow(dfs_common[[1]])}")
} else {
  dfs_common <- dfs
}

# ── Open output PDF ────────────────────────────────────────────────────────────

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
cli::cli_alert_info("Opening {.path {output_file}}")
grDevices::pdf(output_file, width = 10, height = 7)

plots_written <- 0L

# ── Performance profile ────────────────────────────────────────────────────────

if (do_performance_profile) {
  if (length(dfs_common) < 2L) {
    cli::cli_alert_warning(
      "Performance profile requires >= 2 algorithms -- skipping"
    )
  } else {
    cli::cli_h2("Performance profile")
    tryCatch({
      pp <- do.call(
        create_performance_profile_plot,
        c(
          unname(dfs_common),
          list(
            label.timeout    = TEX_LABEL_TIMEOUT,
            label.imbalanced = TEX_LABEL_IMBALANCED,
            label.failed     = TEX_LABEL_FAILED,
            colors           = colors
          )
        )
      )
      print(pp + default_theme)
      plots_written <- plots_written + 1L
    }, error = function(e) {
      cli::cli_alert_danger("Performance profile failed: {e$message}")
    })
  }
}

# ── Speedup plot ───────────────────────────────────────────────────────────────

if (do_speedup) {
  if (length(dfs_common) < 2L) {
    cli::cli_alert_warning(
      "Speedup plot requires >= 2 algorithms -- skipping"
    )
  } else {
    cli::cli_h2(paste0("Speedup plot (baseline: ", algorithms[[1]], ")"))
    tryCatch({
      baseline_df <- dfs_common[[1]]
      rest_dfs    <- dfs_common[-1]

      sp <- do.call(
        create_speedup_plot,
        c(
          unname(rest_dfs),
          list(baseline = baseline_df, colors = colors)
        )
      )
      print(sp + default_theme)
      plots_written <- plots_written + 1L
    }, error = function(e) {
      cli::cli_alert_danger("Speedup plot failed: {e$message}")
    })
  }
}

# ── Running time box plot ──────────────────────────────────────────────────────

if (do_running_time) {
  cli::cli_h2("Running time box plot")
  tryCatch({
    rt <- do.call(
      create_running_time_box_plot,
      c(
        unname(dfs_common),
        list(
          label.timeout    = TEX_LABEL_TIMEOUT,
          label.imbalanced = TEX_LABEL_IMBALANCED,
          label.failed     = TEX_LABEL_FAILED,
          colors           = colors
        )
      )
    )
    print(rt + default_theme)
    plots_written <- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Running time box plot failed: {e$message}")
  })

  cli::cli_h2("Running time per-core box plot")
  tryCatch({
    rtc <- do.call(
      create_running_time_by_core_box_plot,
      c(
        unname(dfs_common),
        list(
          colors = colors
        )
      )
    )
    print(rtc + default_theme)
    plots_written <- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Running time per-core box plot failed: {e$message}")
  })
}

# ── Close PDF ──────────────────────────────────────────────────────────────────

grDevices::dev.off()

if (plots_written > 0L) {
  cli::cli_alert_success(
    "Wrote {plots_written} plot(s) to {.path {output_file}}"
  )
} else {
  cli::cli_alert_warning("No plots were written (check errors above).")
  quit(status = 1L)
}
