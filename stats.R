#!/usr/bin/env Rscript

options(tidyverse.quiet = TRUE)
options(warn = 1)

suppressPackageStartupMessages({
  library(tidyverse, warn.conflicts = FALSE)
  library(jsonlite, warn.conflicts = FALSE)
})

args <- commandArgs(trailingOnly = TRUE)
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
    cat("Usage: stats.R [--results DIR] [--json]\n")
    quit(status = 0)
  } else {
    stop(sprintf("unknown argument: %s", arg), call. = FALSE)
  }
  i <- i + 1L
}

metric_summary <- function(values) {
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

relative_summary <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[!is.na(values) & is.finite(values) & values >= 0]
  count <- length(values)
  if (count == 0L) {
    return(list(count = 0L, ratio = NA_real_))
  }

  ratio <- if (any(values == 0)) 0 else exp(mean(log(values)))
  list(count = count, ratio = unname(ratio))
}

truthy <- function(values) {
  normalized <- tolower(trimws(as.character(values)))
  normalized[is.na(normalized)] <- ""
  normalized %in% c("1", "true", "t", "yes", "y", "failed", "timeout")
}

parse_numeric <- function(values) {
  readr::parse_double(
    as.character(values),
    na = c("", "NA", "NaN", "nan", "NULL", "null"),
    locale = readr::locale(decimal_mark = ".")
  )
}

json_array <- function(values) {
  I(as.list(unname(as.character(values))))
}

empty_result <- function() {
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

print_json <- function(value) {
  cat(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA))
  cat("\n")
}

csv_files <- sort(list.files(results_dir, pattern = "\\.csv$", full.names = TRUE))
if (length(csv_files) == 0L) {
  result <- empty_result()
  if (json_output) {
    print_json(result)
  } else {
    cat(sprintf("No CSV files found under %s\n", normalizePath(results_dir, mustWork = FALSE)))
  }
  quit(status = 0)
}

read_result_csv <- function(path) {
  default_algorithm <- tools::file_path_sans_ext(basename(path))
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

raw_data <- purrr::map_dfr(csv_files, read_result_csv)
if (nrow(raw_data) == 0L) {
  result <- empty_result()
  if (json_output) {
    print_json(result)
  } else {
    cat(sprintf("No result rows found under %s\n", normalizePath(results_dir, mustWork = FALSE)))
  }
  quit(status = 0)
}

original_columns <- names(raw_data)
for (column in c("Cut", "Time", "Imbalance", "Epsilon", "Failed", "Timeout")) {
  if (!(column %in% names(raw_data))) {
    raw_data[[column]] <- NA_character_
  }
}

is_measure_column <- function(name) {
  lower <- tolower(name)
  lower %in% c(
    "algorithm", "cut", "realcut", "time", "imbalance", "timeout", "failed",
    ".file", ".row_in_file"
  ) ||
    stringr::str_detect(lower, "^(time|avg|min|max)") ||
    stringr::str_detect(lower, "(cut|time|imbalance|failed|timeout)$")
}

key_columns <- original_columns[!vapply(original_columns, is_measure_column, logical(1))]
key_columns <- setdiff(key_columns, c("Algorithm", ".file", ".row_in_file"))

make_row_key <- function(df, columns) {
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

data <- raw_data %>%
  dplyr::mutate(
    .failed = truthy(.data$Failed),
    .timeout = truthy(.data$Timeout),
    .cut = parse_numeric(.data$Cut),
    .time = parse_numeric(.data$Time),
    .imbalance = parse_numeric(.data$Imbalance),
    .epsilon = parse_numeric(.data$Epsilon),
    .completed = !.data$.failed & !.data$.timeout,
    .has_balance = !is.na(.data$.imbalance) & !is.na(.data$.epsilon),
    .imbalanced = .data$.completed & .data$.has_balance & (.data$.imbalance > .data$.epsilon + 1e-12),
    .valid_cut = .data$.completed & !is.na(.data$.cut),
    .valid_balanced_cut = .data$.valid_cut & !.data$.imbalanced,
    .valid_time = .data$.completed & !is.na(.data$.time),
    .valid_balanced_time = .data$.valid_time & !.data$.imbalanced
  )
data$.key <- make_row_key(data, key_columns)

algorithms <- sort(unique(data$Algorithm))
algorithm_count <- length(algorithms)

common_keys <- function(df, valid_column) {
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

common_cut_keys <- common_keys(data, ".valid_cut")
common_balanced_cut_keys <- common_keys(data, ".valid_balanced_cut")
common_time_keys <- common_keys(data, ".valid_time")

collapse_metric_values <- function(df, valid_column, value_column) {
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

comparison_matrix <- function(id, title, value_column, valid_column) {
  values <- collapse_metric_values(data, valid_column, value_column)

  cells <- purrr::map(algorithms, function(row_algorithm) {
    purrr::map(algorithms, function(column_algorithm) {
      left <- values %>%
        dplyr::filter(.data$Algorithm == row_algorithm) %>%
        dplyr::transmute(.key = .data$.key, row_value = .data$.value)
      right <- values %>%
        dplyr::filter(.data$Algorithm == column_algorithm) %>%
        dplyr::transmute(.key = .data$.key, column_value = .data$.value)
      joined <- dplyr::inner_join(left, right, by = ".key")
      summary <- relative_summary(joined$row_value / joined$column_value)
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
    algorithms = json_array(algorithms),
    cells = I(cells)
  )
}

comparison_matrices <- list(
  comparison_matrix("time_all", "Time (all)", ".time", ".valid_time"),
  comparison_matrix("time_balanced", "Time (balanced)", ".time", ".valid_balanced_time"),
  comparison_matrix("cut_all", "Cut (all)", ".cut", ".valid_cut"),
  comparison_matrix("cut_balanced", "Cut (balanced only)", ".cut", ".valid_balanced_cut")
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

  cuts_all <- metric_summary(rows$.cut[rows$.valid_cut])
  cuts_balanced <- metric_summary(rows$.cut[rows$.valid_balanced_cut])
  cuts_common_all <- metric_summary(rows$.cut[rows$.valid_cut & rows$.key %in% common_cut_keys])
  cuts_common_balanced <- metric_summary(rows$.cut[rows$.valid_balanced_cut & rows$.key %in% common_balanced_cut_keys])
  times_successful <- metric_summary(rows$.time[rows$.valid_time])
  times_common <- metric_summary(rows$.time[rows$.valid_time & rows$.key %in% common_time_keys])

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
    files = json_array(sort(unique(rows$.file))),
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
  algorithms = algorithm_count,
  rows = nrow(data),
  completed = sum(data$.completed),
  failed = sum(data$.failed),
  timeouts = sum(data$.timeout),
  crashes = sum(data$.failed & !data$.timeout),
  imbalanced = sum(data$.imbalanced),
  missing_cut = sum(data$.completed & is.na(data$.cut)),
  missing_time = sum(data$.completed & is.na(data$.time))
)

result <- list(
  ok = TRUE,
  results_dir = normalizePath(results_dir, mustWork = FALSE),
  summary = summary,
  common = list(
    cut_keys = length(common_cut_keys),
    balanced_cut_keys = length(common_balanced_cut_keys),
    time_keys = length(common_time_keys)
  ),
  comparisons = I(comparison_matrices),
  key_columns = json_array(key_columns),
  algorithms = algorithm_stats
)

if (json_output) {
  print_json(result)
} else {
  cat(sprintf(
    "%-28s %8s %10s %8s %8s %8s %10s %12s %14s %12s\n",
    "Algorithm", "Rows", "Completed", "Failed", "Timeout", "Crash",
    "Imbalanced", "GMean Cut", "Balanced Cut", "GMean Time"
  ))
  for (item in algorithm_stats) {
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
}
