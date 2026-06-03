#!/usr/bin/env Rscript
# mkplots.R -- Main plotting entry point for mkexp2 experiments.
#
# Expected to run inside a Docker container where:
#   /work   = this plots/ submodule directory (working dir; R sources live here)
#   /data   = experiment's results/ directory (read-only CSV files)
#   /output = experiment directory (plots.pdf written here)
# Native execution can override those paths with MKEXP2_PLOTS_DATA_DIR,
# MKEXP2_PLOTS_CACHE_DIR, and an explicit --output path.
#
# Usage:
#   Rscript /work/mkplots.R [--performance-profile] [--speedup] [--running-time]
#                           [--plot <id>]...
#                           [--threads T|NxMxT] [--output <path>]
#                           <source1> [<source2> ...]
#
# When no plot flags are given all three plots are generated (same as specifying
# all three flags).

options(tidyverse.quiet = TRUE)
options(warn = 1)

# ── Parse arguments ────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

do_performance_profile <- FALSE
do_legacy_speedup      <- FALSE
do_running_time        <- FALSE
explicit_plots         <- FALSE
plot_ids               <- character(0)
source_args            <- character(0)
output_file            <- "/output/plots.pdf"
thread_filter          <- NULL

parse_thread_filter <- function(value) {
  parts <- strsplit(value, "x", fixed = TRUE)[[1]]
  if (length(parts) == 1L) {
    parts <- c("1", "1", parts[[1]])
  }
  if (length(parts) != 3L || any(!grepl("^[0-9]+$", parts))) {
    cli::cli_abort("--threads must be T or NxMxT with positive integers, got {.val {value}}")
  }

  parsed <- as.integer(parts)
  if (any(is.na(parsed)) || any(parsed <= 0L)) {
    cli::cli_abort("--threads must be T or NxMxT with positive integers, got {.val {value}}")
  }

  list(
    nodes = parsed[[1]],
    mpis = parsed[[2]],
    threads = parsed[[3]],
    label = paste(parsed, collapse = "x")
  )
}

i <- 1L
while (i <= length(args)) {
  arg <- args[[i]]
  if        (arg == "--performance-profile") {
    do_performance_profile <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--speedup") {
    do_legacy_speedup      <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--running-time") {
    do_running_time        <- TRUE; explicit_plots <- TRUE
  } else if (arg == "--plot") {
    i <- i + 1L
    if (i > length(args)) {
      cli::cli_abort("Missing value for --plot")
    }
    plot_ids <- c(plot_ids, args[[i]])
    explicit_plots <- TRUE
  } else if (startsWith(arg, "--plot=")) {
    plot_ids <- c(plot_ids, substring(arg, 8L))
    explicit_plots <- TRUE
  } else if (arg == "--output") {
    i <- i + 1L
    if (i > length(args)) {
      cli::cli_abort("Missing value for --output")
    }
    output_file <- args[[i]]
  } else if (startsWith(arg, "--output=")) {
    output_file <- substring(arg, 10L)
  } else if (arg == "--threads") {
    i <- i + 1L
    if (i > length(args)) {
      cli::cli_abort("Missing value for --threads")
    }
    thread_filter <- parse_thread_filter(args[[i]])
  } else if (startsWith(arg, "--threads=")) {
    thread_filter <- parse_thread_filter(substring(arg, 11L))
  } else if (!startsWith(arg, "--")) {
    source_args <- c(source_args, arg)
  } else {
    cli::cli_alert_warning("Unknown argument ignored: {arg}")
  }
  i <- i + 1L
}

if (!explicit_plots) {
  do_performance_profile <- TRUE
  do_legacy_speedup      <- TRUE
  do_running_time        <- TRUE
}

if (length(source_args) == 0L) {
  cli::cli_abort(c(
    "No plot sources specified.",
    "i" = "Usage: mkplots.R [--plot <id>] [--output <path>] <source1> [<source2> ...]"
  ))
}

parse_source_arg <- function(value) {
  if (grepl("=", value, fixed = TRUE)) {
    parts <- strsplit(value, "=", fixed = TRUE)[[1]]
    alias <- parts[[1]]
    source <- paste(parts[-1], collapse = "=")
  } else {
    source <- value
    if (grepl("\\.csv$", source, ignore.case = TRUE) || grepl("/", source, fixed = TRUE)) {
      alias <- tools::file_path_sans_ext(basename(source))
    } else {
      alias <- source
    }
  }
  if (!nzchar(alias) || !nzchar(source)) {
    cli::cli_abort("Invalid plot source {.val {value}}")
  }
  list(alias = alias, source = source)
}

source_specs <- lapply(source_args, parse_source_arg)
algorithms <- vapply(source_specs, function(spec) spec$alias, character(1L))

cli::cli_alert_info("Sources    : {.val {algorithms}}")
cli::cli_alert_info("Output     : {.path {output_file}}")
if (!is.null(thread_filter)) {
  cli::cli_alert_info("Threads    : {.val {thread_filter$label}}")
}

# ── Source plot library ────────────────────────────────────────────────────────
# Working directory inside Docker is /work (the plots submodule).

source("R/common.R")
source("R/performance_profile_plot.R")
source("R/speedup_plot.R")
source("R/running_time_box_plot.R")
source("R/running_time_by_core_box_plot.R")
source("R/relative_by_graph_grid_plot.R")

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

dfs <- lapply(source_specs, function(spec) {
  cli::cli_alert_info("Loading {.val {spec$alias}}")
  tryCatch(
    if (!is.null(thread_filter)) {
      load_dataset(spec$source, spec$alias, cache = FALSE, topology_filter = thread_filter)
    } else {
      load_dataset(spec$source, spec$alias, cache = FALSE)
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to load {.val {spec$alias}}: {e$message}")
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

render_performance_profile <- function() {
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
      plots_written <<- plots_written + 1L
    }, error = function(e) {
      cli::cli_alert_danger("Performance profile failed: {e$message}")
    })
  }
}

render_legacy_speedup <- function() {
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
      plots_written <<- plots_written + 1L
    }, error = function(e) {
      cli::cli_alert_danger("Speedup plot failed: {e$message}")
    })
  }
}

render_running_time_box <- function() {
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
    plots_written <<- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Running time box plot failed: {e$message}")
  })
}

render_running_time_by_core <- function() {
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
    plots_written <<- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Running time per-core box plot failed: {e$message}")
  })
}

render_relative_cut_graph_grid <- function() {
  if (length(dfs_common) < 2L) {
    cli::cli_alert_warning("Relative cut graph grid requires >= 2 algorithms -- skipping")
    return()
  }
  cli::cli_h2("Relative cut graph grid")
  tryCatch({
    rcg <- do.call(
      create_relative_by_graph_grid_plot,
      c(
        unname(dfs_common[-1]),
        list(
          baseline = dfs_common[[1]],
          metric = "cut",
          colors = colors,
          levels = algorithms
        )
      )
    )
    print(rcg + default_theme + relative_by_graph_grid_theme())
    plots_written <<- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Relative cut graph grid failed: {e$message}")
  })
}

render_relative_time_graph_grid <- function() {
  if (length(dfs_common) < 2L) {
    cli::cli_alert_warning("Relative running time graph grid requires >= 2 algorithms -- skipping")
    return()
  }
  cli::cli_h2("Relative running time graph grid")
  tryCatch({
    rtg <- do.call(
      create_relative_by_graph_grid_plot,
      c(
        unname(dfs_common[-1]),
        list(
          baseline = dfs_common[[1]],
          metric = "time",
          colors = colors,
          levels = algorithms
        )
      )
    )
    print(rtg + default_theme + relative_by_graph_grid_theme())
    plots_written <<- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Relative running time graph grid failed: {e$message}")
  })
}

render_core_speedup <- function() {
  if (length(dfs) != 1L) {
    cli::cli_alert_warning("Speedup requires exactly one source -- skipping")
    return()
  }
  cli::cli_h2("Speedup")
  tryCatch({
    core_colors <- c()
    sp <- create_core_speedup_plot(dfs[[1]], colors = core_colors)
    print(sp + default_theme)
    plots_written <<- plots_written + 1L
  }, error = function(e) {
    cli::cli_alert_danger("Speedup failed: {e$message}")
  })
}

# ── Plot rendering ─────────────────────────────────────────────────────────────

for (plot_id in plot_ids) {
  if (plot_id == "performance-profile") {
    render_performance_profile()
  } else if (plot_id == "running-time-box") {
    render_running_time_box()
  } else if (plot_id == "running-time-by-core") {
    render_running_time_by_core()
  } else if (plot_id == "relative-cut-graph-grid") {
    render_relative_cut_graph_grid()
  } else if (plot_id == "relative-time-graph-grid") {
    render_relative_time_graph_grid()
  } else if (plot_id == "speedup") {
    render_core_speedup()
  } else {
    cli::cli_alert_danger("Unknown plot type: {.val {plot_id}}")
  }
}

if (do_performance_profile) {
  render_performance_profile()
}
if (do_legacy_speedup) {
  render_legacy_speedup()
}
if (do_running_time) {
  render_running_time_box()
  render_running_time_by_core()
  render_relative_cut_graph_grid()
  render_relative_time_graph_grid()
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
