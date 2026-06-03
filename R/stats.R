#!/usr/bin/env Rscript

options(tidyverse.quiet = TRUE)
options(warn = 1)

suppressPackageStartupMessages({
  library(tidyverse, warn.conflicts = FALSE)
  library(jsonlite, warn.conflicts = FALSE)
})

#' Summarize a non-negative metric with arithmetic and geometric statistics.
#'
#' @param values Numeric-like values. Missing, infinite, and negative values are
#'   ignored.
#' @return A list with count, geometric mean, arithmetic mean, minimum, and
#'   maximum.
stats_metric_summary <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values) & is.finite(values) & values >= 0]
  count <- length(values)
  if (count == 0L) {
    return(list(count = 0L, gmean = NA_real_, mean = NA_real_, min = NA_real_, max = NA_real_))
  }

  gmean <- if (any(values == 0)) 0 else exp(mean(log(values)))
  list(
    count = count,
    gmean = unname(gmean),
    mean = unname(mean(values)),
    min = unname(min(values)),
    max = unname(max(values))
  )
}

#' Summarize non-negative ratios with a geometric mean.
#'
#' @param values Numeric-like ratio values. Missing, infinite, and negative
#'   values are ignored.
#' @return A list with count and geometric-mean ratio.
stats_relative_summary <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values) & is.finite(values) & values >= 0]
  count <- length(values)
  if (count == 0L) {
    return(list(count = 0L, ratio = NA_real_))
  }

  ratio <- if (any(values == 0)) 0 else exp(mean(log(values)))
  list(count = count, ratio = unname(ratio))
}

stats_truthy <- function(values) {
  normalized <- tolower(trimws(as.character(values)))
  normalized[is.na(normalized)] <- ""
  normalized %in% c("1", "true", "t", "yes", "y", "failed", "timeout")
}

stats_parse_numeric <- function(values) {
  readr::parse_double(
    as.character(values),
    na = c("", "NA", "NaN", "nan", "NULL", "null"),
    locale = readr::locale(decimal_mark = ".")
  )
}

stats_json_array <- function(values) {
  I(as.list(unname(as.character(values))))
}

stats_empty_result <- function(results_dir = "results") {
  list(
    ok = TRUE,
    results_dir = normalizePath(results_dir, mustWork = FALSE),
    summary = list(
      algorithms = 0L,
      rows = 0L,
      completed = 0L,
      failed = 0L,
      timeouts = 0L,
      crashes = 0L,
      imbalanced = 0L,
      missing_cut = 0L,
      missing_time = 0L
    ),
    common = list(cut_keys = 0L, balanced_cut_keys = 0L, time_keys = 0L),
    comparisons = I(list()),
    algorithms = I(list())
  )
}

#' Print a stats result object as mkexp2-compatible JSON.
#'
#' @param value Result object from `create_stats_summary()`.
#' @return Invisibly returns `value`.
print_stats_json <- function(value) {
  cat(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA))
  cat("\n")
  invisible(value)
}

#' Read one mkexp2 result CSV.
#'
#' @param path CSV path.
#' @param algorithm Optional algorithm name used when the CSV has no Algorithm
#'   column or blank Algorithm cells. Defaults to the CSV basename.
#' @return A tibble with Algorithm, .file, and .row_in_file metadata.
read_result_csv <- function(path, algorithm = NULL) {
  default_algorithm <- if (!is.null(algorithm) && nzchar(algorithm)) {
    algorithm
  } else {
    tools::file_path_sans_ext(basename(path))
  }
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE,
    name_repair = "unique"
  )
  if (!("Algorithm" %in% names(df))) {
    df$Algorithm <- default_algorithm
  }
  df %>%
    dplyr::mutate(
      Algorithm = dplyr::if_else(
        is.na(.data$Algorithm) | trimws(.data$Algorithm) == "",
        default_algorithm,
        trimws(.data$Algorithm)
      ),
      .file = basename(path),
      .row_in_file = dplyr::row_number()
    )
}

#' Load all mkexp2 result CSVs from a directory.
#'
#' @param results_dir Directory containing `*.csv` result files.
#' @return A tibble containing all rows, or an empty tibble when no CSV files are
#'   present.
load_result_csvs <- function(results_dir = "results") {
  csv_files <- sort(list.files(results_dir, pattern = "\\.csv$", full.names = TRUE))
  if (length(csv_files) == 0L) {
    return(tibble::tibble())
  }
  purrr::map_dfr(csv_files, read_result_csv)
}

stats_normalize_input <- function(df, name, index) {
  if (is.null(df) || nrow(df) == 0L) {
    return(tibble::tibble())
  }

  df <- tibble::as_tibble(df)
  default_algorithm <- if (!is.null(name) && nzchar(name)) {
    name
  } else if ("Algorithm" %in% names(df) && any(!is.na(df$Algorithm) & trimws(as.character(df$Algorithm)) != "")) {
    trimws(as.character(df$Algorithm[which(!is.na(df$Algorithm) & trimws(as.character(df$Algorithm)) != "")[1]]))
  } else {
    paste0("Algorithm ", index)
  }

  if (!("Algorithm" %in% names(df))) {
    df$Algorithm <- default_algorithm
  }
  if (!(".file" %in% names(df))) {
    df$.file <- paste0(default_algorithm, ".csv")
  }
  if (!(".row_in_file" %in% names(df))) {
    df$.row_in_file <- seq_len(nrow(df))
  }

  df %>%
    dplyr::mutate(
      Algorithm = dplyr::if_else(
        is.na(.data$Algorithm) | trimws(as.character(.data$Algorithm)) == "",
        default_algorithm,
        trimws(as.character(.data$Algorithm))
      )
    )
}

stats_is_measure_column <- function(name) {
  lower <- tolower(name)
  lower %in% c(
    "algorithm", "cut", "realcut", "time", "imbalance", "timeout", "failed",
    ".file", ".row_in_file"
  ) ||
    stringr::str_detect(lower, "^(time|avg|min|max)") ||
    stringr::str_detect(lower, "(cut|time|imbalance|failed|timeout)$")
}

stats_make_row_key <- function(df, columns) {
  if (length(columns) == 0L) {
    return(paste0("__row=", df$.row_in_file))
  }
  parts <- purrr::map(columns, function(column) {
    values <- as.character(df[[column]])
    values[is.na(values)] <- ""
    paste0(column, "=", values)
  })
  do.call(paste, c(parts, sep = "|"))
}

stats_common_keys <- function(df, algorithms, valid_column) {
  algorithm_count <- length(algorithms)
  if (algorithm_count == 0L) {
    return(character(0))
  }
  df %>%
    dplyr::filter(.data[[valid_column]]) %>%
    dplyr::distinct(.data$Algorithm, .data$.key) %>%
    dplyr::count(.data$.key, name = ".n_algorithms") %>%
    dplyr::filter(.data$.n_algorithms == algorithm_count) %>%
    dplyr::pull(.data$.key)
}

stats_collapse_metric_values <- function(df, valid_column, value_column) {
  df %>%
    dplyr::filter(.data[[valid_column]]) %>%
    dplyr::mutate(.value = suppressWarnings(as.numeric(.data[[value_column]]))) %>%
    dplyr::filter(!is.na(.data$.value), is.finite(.data$.value), .data$.value >= 0) %>%
    dplyr::group_by(.data$Algorithm, .data$.key) %>%
    dplyr::summarise(
      .value = {
        values <- .data$.value
        if (any(values == 0)) 0 else exp(mean(log(values)))
      },
      .rows = dplyr::n(),
      .groups = "drop"
    )
}

#' Build one pairwise comparison matrix.
#'
#' @param data Prepared result data from `create_stats_summary()`.
#' @param algorithms Algorithm names to use for matrix rows and columns.
#' @param id Stable machine-readable matrix id.
#' @param title Human-readable matrix title.
#' @param value_column Numeric prepared-data column to compare.
#' @param valid_column Logical prepared-data column selecting usable rows.
#' @return A mkexp2 stats comparison matrix object.
create_stats_comparison_matrix <- function(data, algorithms, id, title, value_column, valid_column) {
  values <- stats_collapse_metric_values(data, valid_column, value_column)

  cells <- purrr::map(algorithms, function(row_algorithm) {
    purrr::map(algorithms, function(column_algorithm) {
      left <- values %>%
        dplyr::filter(.data$Algorithm == row_algorithm) %>%
        dplyr::transmute(.key = .data$.key, row_value = .data$.value)
      right <- values %>%
        dplyr::filter(.data$Algorithm == column_algorithm) %>%
        dplyr::transmute(.key = .data$.key, column_value = .data$.value)
      joined <- dplyr::inner_join(left, right, by = ".key")
      summary <- stats_relative_summary(joined$row_value / joined$column_value)
      list(
        row_algorithm = row_algorithm,
        column_algorithm = column_algorithm,
        count = summary$count,
        ratio = summary$ratio
      )
    })
  }) %>% purrr::flatten()

  list(
    id = id,
    title = title,
    algorithms = stats_json_array(algorithms),
    cells = I(cells)
  )
}

#' Create mkexp2 stats from result data frames or a results directory.
#'
#' @param ... Result data frames, usually one per algorithm. Named data frames use
#'   their name as the fallback Algorithm value.
#' @param results_dir Directory to scan for `*.csv` files when no data frames are
#'   supplied. Also recorded in the output JSON.
#' @param key_columns Optional instance identity columns. When omitted, identity
#'   columns are inferred by excluding metric and metadata columns.
#' @return The mkexp2 stats result object used by `mkexp2 stats --json`.
create_stats_summary <- function(..., results_dir = "results", key_columns = NULL) {
  dfs <- list(...)
  df_names <- names(dfs)

  raw_data <- if (length(dfs) > 0L) {
    purrr::map2_dfr(seq_along(dfs), dfs, function(index, df) {
      name <- if (!is.null(df_names) && nzchar(df_names[[index]])) {
        df_names[[index]]
      } else {
        NULL
      }
      stats_normalize_input(df, name, index)
    })
  } else {
    load_result_csvs(results_dir)
  }

  if (nrow(raw_data) == 0L) {
    return(stats_empty_result(results_dir))
  }

  original_columns <- names(raw_data)
  for (column in c("Cut", "Time", "Imbalance", "Epsilon", "Failed", "Timeout")) {
    if (!(column %in% names(raw_data))) {
      raw_data[[column]] <- NA_character_
    }
  }

  if (is.null(key_columns)) {
    key_columns <- original_columns[!vapply(original_columns, stats_is_measure_column, logical(1))]
    key_columns <- setdiff(key_columns, c("Algorithm", ".file", ".row_in_file"))
  }

  data <- raw_data %>%
    dplyr::mutate(
      .failed = stats_truthy(.data$Failed),
      .timeout = stats_truthy(.data$Timeout),
      .cut = stats_parse_numeric(.data$Cut),
      .time = stats_parse_numeric(.data$Time),
      .imbalance = stats_parse_numeric(.data$Imbalance),
      .epsilon = stats_parse_numeric(.data$Epsilon),
      .completed = !.data$.failed & !.data$.timeout,
      .has_balance = !is.na(.data$.imbalance) & !is.na(.data$.epsilon),
      .imbalanced = .data$.completed & .data$.has_balance & (.data$.imbalance > .data$.epsilon + 1e-12),
      .valid_cut = .data$.completed & !is.na(.data$.cut),
      .valid_balanced_cut = .data$.valid_cut & !.data$.imbalanced,
      .valid_time = .data$.completed & !is.na(.data$.time),
      .valid_balanced_time = .data$.valid_time & !.data$.imbalanced
    )
  data$.key <- stats_make_row_key(data, key_columns)

  algorithms <- sort(unique(data$Algorithm))

  common_cut_keys <- stats_common_keys(data, algorithms, ".valid_cut")
  common_balanced_cut_keys <- stats_common_keys(data, algorithms, ".valid_balanced_cut")
  common_time_keys <- stats_common_keys(data, algorithms, ".valid_time")

  comparison_matrices <- list(
    create_stats_comparison_matrix(data, algorithms, "time_all", "Time (all)", ".time", ".valid_time"),
    create_stats_comparison_matrix(data, algorithms, "time_balanced", "Time (balanced)", ".time", ".valid_balanced_time"),
    create_stats_comparison_matrix(data, algorithms, "cut_all", "Cut (all)", ".cut", ".valid_cut"),
    create_stats_comparison_matrix(data, algorithms, "cut_balanced", "Cut (balanced only)", ".cut", ".valid_balanced_cut")
  )

  algorithm_stats <- purrr::map(algorithms, function(algorithm) {
    rows <- data %>% dplyr::filter(.data$Algorithm == algorithm)
    quality <- list(
      rows = nrow(rows),
      completed = sum(rows$.completed),
      failed = sum(rows$.failed),
      timeouts = sum(rows$.timeout),
      crashes = sum(rows$.failed & !rows$.timeout),
      imbalanced = sum(rows$.imbalanced),
      missing_cut = sum(rows$.completed & is.na(rows$.cut)),
      missing_time = sum(rows$.completed & is.na(rows$.time))
    )

    cuts_all <- stats_metric_summary(rows$.cut[rows$.valid_cut])
    cuts_balanced <- stats_metric_summary(rows$.cut[rows$.valid_balanced_cut])
    cuts_common_all <- stats_metric_summary(rows$.cut[rows$.valid_cut & rows$.key %in% common_cut_keys])
    cuts_common_balanced <- stats_metric_summary(rows$.cut[rows$.valid_balanced_cut & rows$.key %in% common_balanced_cut_keys])
    times_successful <- stats_metric_summary(rows$.time[rows$.valid_time])
    times_common <- stats_metric_summary(rows$.time[rows$.valid_time & rows$.key %in% common_time_keys])

    list(
      algorithm = algorithm,
      rows = quality$rows,
      completed = quality$completed,
      failed = quality$failed,
      timeouts = quality$timeouts,
      crashes = quality$crashes,
      imbalanced = quality$imbalanced,
      missing_cut = quality$missing_cut,
      missing_time = quality$missing_time,
      cut_count = cuts_all$count,
      avg_cut = cuts_all$gmean,
      time_count = times_successful$count,
      avg_time = times_successful$gmean,
      files = stats_json_array(sort(unique(rows$.file))),
      quality = quality,
      cuts = list(
        all_successful = cuts_all,
        balanced = cuts_balanced,
        common_all_successful = cuts_common_all,
        common_balanced = cuts_common_balanced
      ),
      times = list(
        successful = times_successful,
        common_successful = times_common
      )
    )
  })

  summary <- list(
    algorithms = length(algorithms),
    rows = nrow(data),
    completed = sum(data$.completed),
    failed = sum(data$.failed),
    timeouts = sum(data$.timeout),
    crashes = sum(data$.failed & !data$.timeout),
    imbalanced = sum(data$.imbalanced),
    missing_cut = sum(data$.completed & is.na(data$.cut)),
    missing_time = sum(data$.completed & is.na(data$.time))
  )

  list(
    ok = TRUE,
    results_dir = normalizePath(results_dir, mustWork = FALSE),
    summary = summary,
    common = list(
      cut_keys = length(common_cut_keys),
      balanced_cut_keys = length(common_balanced_cut_keys),
      time_keys = length(common_time_keys)
    ),
    comparisons = I(comparison_matrices),
    key_columns = stats_json_array(key_columns),
    algorithms = algorithm_stats
  )
}

#' Print a mkexp2 stats table.
#'
#' @param result Result object from `create_stats_summary()`.
#' @return Invisibly returns `result`.
print_stats_table <- function(result) {
  cat(sprintf(
    "%-28s %8s %10s %8s %8s %8s %10s %12s %14s %12s\n",
    "Algorithm", "Rows", "Completed", "Failed", "Timeout", "Crash",
    "Imbalanced", "GMean Cut", "Balanced Cut", "GMean Time"
  ))
  for (item in result$algorithms) {
    cat(sprintf(
      "%-28s %8d %10d %8d %8d %8d %10d %12.6g %14.6g %12.6g\n",
      item$algorithm,
      item$rows,
      item$completed,
      item$failed,
      item$timeouts,
      item$crashes,
      item$imbalanced,
      item$cuts$all_successful$gmean,
      item$cuts$balanced$gmean,
      item$times$successful$gmean
    ))
  }
  invisible(result)
}

parse_stats_args <- function(args) {
  results_dir <- "results"
  json_output <- FALSE

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--json") {
      json_output <- TRUE
    } else if (arg == "--results") {
      i <- i + 1L
      if (i > length(args)) {
        stop("missing value for --results", call. = FALSE)
      }
      results_dir <- args[[i]]
    } else if (startsWith(arg, "--results=")) {
      results_dir <- substring(arg, 11L)
    } else if (arg %in% c("-h", "--help")) {
      cat("Usage: mkexp.R stats [--results DIR] [--json]\n")
      quit(status = 0)
    } else {
      stop(sprintf("unknown argument: %s", arg), call. = FALSE)
    }
    i <- i + 1L
  }

  list(results_dir = results_dir, json = json_output)
}

#' Run the mkexp2 stats command-line integration.
#'
#' @param args Command-line arguments after the `stats` subcommand.
#' @return Invisibly returns the stats result object.
run_mkexp_stats <- function(args = character(0)) {
  opts <- parse_stats_args(args)
  csv_files <- sort(list.files(opts$results_dir, pattern = "\\.csv$", full.names = TRUE))
  result <- create_stats_summary(results_dir = opts$results_dir)

  if (result$summary$rows == 0L && !opts$json) {
    if (length(csv_files) == 0L) {
      cat(sprintf("No CSV files found under %s\n", normalizePath(opts$results_dir, mustWork = FALSE)))
    } else {
      cat(sprintf("No result rows found under %s\n", normalizePath(opts$results_dir, mustWork = FALSE)))
    }
    return(invisible(result))
  }

  if (opts$json) {
    print_stats_json(result)
  } else {
    print_stats_table(result)
  }

  invisible(result)
}
