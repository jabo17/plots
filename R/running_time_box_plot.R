#!/usr/bin/env Rscript
show_timeout <- \(df, option) option == "always" || (option == "auto" && any(df$Timeout))
show_imbalanced <- \(df, option) option == "always" || (option == "auto" && any(df$Imbalanced))
show_failed <- \(df, option) option == "always" || (option == "auto" && any(df$Failed))

#' Plot per-instance running times as algorithm box plots.
#'
#' @param ... Normalized result data frames with identical primary keys.
#' @param column.time,column.algorithm,column.timeout,column.imbalanced,column.failed
#'   Source column names.
#' @param primary_key Columns used to align rows.
#' @param exclude.imbalanced If `TRUE`, move imbalanced rows to the error tick
#'   band instead of including their running time.
#' @param tick.timeout,tick.imbalanced,tick.failed `"auto"`, `"always"`, or any
#'   other value to hide each error tick.
#' @param tick.errors.space_below,tick.errors.space_between Log-axis spacing for
#'   error ticks.
#' @param tex If `TRUE`, use TeX math and symbolic error labels.
#' @param label.timeout,label.imbalanced,label.failed Labels for error ticks.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param annotate `"none"`, `"minimal"`, or `"extensive"` geometric-mean labels.
#' @param annotate.position Divider controlling minimal annotation y-position.
#' @param position.y Y-axis position.
#' @param plot.xlab,plot.ylab Axis labels; set to `NA` to omit.
create_running_time_box_plot <- \(
    ...,
    column.time = "AvgTime",
    column.algorithm = "Algorithm",
    column.timeout = "Timeout",
    column.imbalanced = "Imbalanced",
    column.failed = "Failed",
    primary_key = c("Graph", "K"),
    exclude.imbalanced = FALSE,
    tick.timeout = "auto",
    tick.imbalanced = "auto",
    tick.failed = "auto",
    tick.errors.space_below = 0.8,
    tick.errors.space_between = 0.8,
    tex = FALSE,
    label.timeout = plot_label(tex, TEX_LABEL_TIMEOUT, PDF_LABEL_TIMEOUT),
    label.imbalanced = plot_label(tex, TEX_LABEL_IMBALANCED, PDF_LABEL_IMBALANCED),
    label.failed = plot_label(tex, TEX_LABEL_FAILED, PDF_LABEL_FAILED),
    colors = c(),
    levels = c(),
    annotate = "minimal", # none, minimal, extensive
    annotate.position = 2,
    position.y = "left",
    plot.xlab = "Algorithm",
    plot.ylab = "Time [s]"
) {
    all_dfs <- list(...)

    if (length(all_dfs) < 1) {
        cli::cli_abort("Need at least one data frames for plotting a running time box plot.")
    }

    # Run some basic sanity checks against the data frames
    for (i in 1 : length(all_dfs)) {
        # Sort the data; replace 0 by 1 to ensure that zero-cuts do not crash our code
        df <- all_dfs[[i]] %>% dplyr::arrange_at(primary_key) 
        all_dfs[[i]] <- df

        if (!(column.algorithm %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.algorithm}} missing from data frame no. {.val {i}}.")
        }

        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]
        if (!all(algorithms == algorithm)) {
            cli::cli_abort("Rows for multiple algorithms in the same data frame no. {.val {i}}: {.val {unique(algorithms)}}")
        }

        if (!(column.time %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.time}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.timeout %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.timeout}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.imbalanced %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.imbalanced}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!column.failed %in% colnames(df)) {
            cli::cli_abort("Column {.field {column.failed}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }

        if (0 %in% df[[column.time]]) {
            cli::cli_abort("Column {.field {column.time}} of data frame no. {.val {i}} (algorithm {.val {algorithm}}) contains {.val 0} values.")
        }
        if (-Inf %in% df[[column.time]]) {
            cli::cli_abort("Column {.field {column.time}} of data frame no. {.val {i}} (algorithm {.val {algorithm}}) contains {.val -Inf} values.")
        }

        if (nrow(df[, primary_key]) != nrow(all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Number of rows for the primary keys in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows for the primary keys in the first data frame.")
        }
        if (!all.equal(df[, primary_key], all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
        }
    }

    data <- purrr::map_dfr(all_dfs, \(df) df %>%
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time),
            Timeout = rlang::sym(column.timeout),
            Imbalanced = rlang::sym(column.imbalanced),
            Failed = rlang::sym(column.failed)
        ) %>%
        dplyr::mutate(
            PK = dplyr::row_number(),
            Imbalanced = Imbalanced & exclude.imbalanced,
            JitterTime = Time,
            AnnotationTime = Time
        )
    )

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels, ordered = TRUE)
    }

    # Find max time
    min_max_time <- data %>%
        dplyr::filter(!Timeout & !Imbalanced & !Failed) %>%
        dplyr::summarize(Max = max(Time), Min = min(Time))
    max_time_log10 <- ceiling(log10(min_max_time$Max))
    min_time_log10 <- ceiling(log10(min_max_time$Min))
    max_time_exp10 <- 10 ^ max_time_log10
    min_time_exp10 <- 10 ^ min_time_log10

    # Create ticks
    y_breaks <- 10 ^ seq(min_time_log10, max_time_log10, by = 1)
    y_labels <- plot_power_label(10, log10(y_breaks), tex)

    show_imbalanced_tick <- show_imbalanced(data, tick.imbalanced)
    show_timeout_tick <- show_timeout(data, tick.timeout)
    show_failed_tick <- show_failed(data, tick.failed)
    show_error_ticks <- show_imbalanced_tick || show_timeout_tick || show_failed_tick

    offset <- tick.errors.space_below - tick.errors.space_below
    if (show_imbalanced_tick) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.imbalanced)
    }

    # Mark imbalanced cuts as imbalanced
    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(exclude.imbalanced & Imbalanced & !Timeout, 10 ^ (max_time_log10 + offset), JitterTime),
        Time = ifelse(exclude.imbalanced & Imbalanced & !Timeout, NA, Time)
    )

    if (show_timeout_tick) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.timeout)
    }

    # Mark timeout cuts as timeouts
    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(Timeout, 10 ^ (max_time_log10 + offset), JitterTime)
    )

    if (show_failed_tick) {
        offset <- offset + tick.errors.space_between
        y_breaks <- c(y_breaks, 10 ^ (max_time_log10 + offset))
        y_labels <- c(y_labels, label.failed)
    }

    # Mark failed cuts as failed
    data <- data %>% dplyr::mutate(
        JitterTime = ifelse(Failed & !Timeout & !Imbalanced, 10 ^ (max_time_log10 + offset), JitterTime),
        Time = ifelse(Failed & !Timeout & !Imbalanced, NA, Time)
    )

    data <- data %>%
        dplyr::group_by(PK) %>%
        dplyr::mutate(.AllOk = all(!is.na(Time))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ComparableTime = ifelse(.AllOk, Time, NA)) %>%
        dplyr::select(-.AllOk)

    annotation <- data %>%
        dplyr::group_by(Algorithm) %>%
        dplyr::summarize(
            Num = dplyr::n(),
            NumFeasibles = sum(!is.na(Time)),
            NumCommon = sum(!is.na(ComparableTime)),
            GmeanFeasibles = exp(mean(log(Time), na.rm = TRUE)),
            GmeanCommon = exp(mean(log(ComparableTime), na.rm = TRUE)), 
            .groups = "drop"
        )

    p <- ggplot2::ggplot(data, ggplot2::aes(x = Algorithm, y = JitterTime)) +
        ggplot2::geom_jitter(
            ggplot2::aes(color = Algorithm, fill = Algorithm),
            size = 0.75,
            alpha = 0.33,
            pch = 21,
            width = 0.3
        ) +
        ggplot2::stat_boxplot(
            ggplot2::aes(y = ComparableTime, color = Algorithm), 
            geom = "errorbar", width = 0.6,
            na.rm = TRUE
        ) +
        ggplot2::geom_boxplot(
            ggplot2::aes(y = ComparableTime, color = Algorithm), 
            outlier.shape = NA, 
            alpha = 0.5,
            na.rm = TRUE
        ) +
        ggplot2::scale_y_continuous(
            trans = "log10", 
            breaks = y_breaks, 
            labels = y_labels, 
            position = position.y
        )

    if (annotate == "extensive") {
        p <- p + ggplot2::geom_text(
            ggplot2::aes(
                x = Algorithm, 
                y = min(data$JitterTime) / 2,
                label = sprintf(
                    "Cmp: %.2f s (%d), All: %.2f s (%d)",
                    GmeanCommon, 
                    NumCommon,
                    GmeanFeasibles,
                    NumFeasibles
                ),
                vjust = 0.5
            ), 
            annotation, 
            size = 2.5
        )
    } else if (annotate == "minimal") {
        p <- p + ggplot2::geom_text(
            ggplot2::aes(
                x = Algorithm, 
                y = min(data$JitterTime) / annotate.position,
                label = sprintf("%.2f s", GmeanCommon),
                vjust = 0.5
            ), 
            annotation, 
            size = 2.5
        )
    }

    if (show_error_ticks) {
        p <- p + ggplot2::geom_hline(yintercept = 10 ^ (max_time_log10 + tick.errors.space_below / 2))
    }

    # Set colors
    if (length(colors) > 0) {
        p <- p +
            ggplot2::scale_color_manual(name = "Algorithm", values = colors) +
            ggplot2::scale_fill_manual(name = "Algorithm", values = colors)
    }

    if (!is.na(plot.xlab)) {
        p <- p + ggplot2::xlab(plot.xlab)
    }
    if (!is.na(plot.ylab)) {
        p <- p + ggplot2::ylab(plot.ylab)
    }

    p
}
