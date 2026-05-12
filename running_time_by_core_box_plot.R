#!/usr/bin/env Rscript

create_running_time_by_core_box_plot <- \(
    ...,
    column.time = "AvgTime",
    column.algorithm = "Algorithm",
    column.cores = "Cores",
    column.timeout = "Timeout",
    column.imbalanced = "Imbalanced",
    column.failed = "Failed",
    primary_key = c("Graph", "K", "Epsilon", "Cores"),
    exclude.imbalanced = FALSE,
    colors = c(),
    levels = c(),
    annotate = "minimal", # none, minimal
    annotate.position = 2,
    plot.xlab = "Cores",
    plot.ylab = "Time [s]"
) {
    all_dfs <- list(...)

    if (length(all_dfs) < 1) {
        cli::cli_abort("Need at least one data frame for plotting a running time per-core box plot.")
    }

    all_keys <- unique(c(
        primary_key,
        column.algorithm,
        column.cores,
        column.time,
        column.timeout,
        column.imbalanced,
        column.failed
    ))
    for (i in 1 : length(all_dfs)) {
        missing <- setdiff(all_keys, colnames(all_dfs[[i]]))
        if (length(missing) > 0) {
            algorithm <- if (column.algorithm %in% colnames(all_dfs[[i]])) {
                all_dfs[[i]][[column.algorithm]][1]
            } else {
                paste0("data frame no. ", i)
            }
            cli::cli_abort(
                "Columns missing from {.val {algorithm}} for running time per-core box plot: {.field {missing}}"
            )
        }
    }

    data <- purrr::map_dfr(all_dfs, \(df) {
        selected <- df %>% dplyr::select(dplyr::all_of(all_keys))
        selected$Algorithm <- selected[[column.algorithm]]
        selected$Time <- selected[[column.time]]
        selected$Timeout <- selected[[column.timeout]]
        selected$Imbalanced <- selected[[column.imbalanced]]
        selected$Failed <- selected[[column.failed]]
        selected$Cores <- selected[[column.cores]]
        selected %>%
            dplyr::mutate(
                Imbalanced = Imbalanced & exclude.imbalanced,
                Time = ifelse(Failed | Imbalanced, NA, Time)
            )
    })

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels, ordered = TRUE)
    }

    data <- data %>%
        dplyr::mutate(
            Cores = as.integer(Cores),
            CoreLabel = factor(Cores, levels = sort(unique(Cores)))
        )

    comparable_keys <- data %>%
        dplyr::filter(!is.na(Time)) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(primary_key))) %>%
        dplyr::summarize(
            NumAlgorithms = dplyr::n_distinct(Algorithm),
            .groups = "drop"
        ) %>%
        dplyr::filter(NumAlgorithms == length(all_dfs)) %>%
        dplyr::select(dplyr::all_of(primary_key))

    data <- data %>%
        dplyr::inner_join(comparable_keys, by = primary_key)

    if (nrow(data) == 0) {
        cli::cli_abort("No comparable per-core running time data remains after filtering failures.")
    }
    if (any(data$Time == 0, na.rm = TRUE) || any(data$Time == -Inf, na.rm = TRUE)) {
        cli::cli_abort("Running time per-core box plot requires positive finite running times.")
    }

    y_range <- data %>%
        dplyr::filter(!is.na(Time)) %>%
        dplyr::summarize(Max = max(Time), Min = min(Time))
    max_time_log10 <- ceiling(log10(y_range$Max))
    min_time_log10 <- floor(log10(y_range$Min))
    y_breaks <- 10 ^ seq(min_time_log10, max_time_log10, by = 1)
    y_labels <- sapply(y_breaks, \(val) paste0("$10^{", log10(val), "}$"))

    annotation <- data %>%
        dplyr::group_by(CoreLabel, Algorithm) %>%
        dplyr::summarize(
            Gmean = exp(mean(log(Time), na.rm = TRUE)),
            .groups = "drop"
        )

    dodge <- ggplot2::position_dodge(width = 0.8)

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = CoreLabel, y = Time, color = Algorithm)
    ) +
        ggplot2::geom_jitter(
            ggplot2::aes(fill = Algorithm),
            position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
            size = 0.65,
            alpha = 0.25,
            pch = 21
        ) +
        ggplot2::stat_boxplot(
            geom = "errorbar",
            width = 0.35,
            position = dodge,
            na.rm = TRUE
        ) +
        ggplot2::geom_boxplot(
            outlier.shape = NA,
            alpha = 0.5,
            position = dodge,
            na.rm = TRUE
        ) +
        ggplot2::scale_y_continuous(
            trans = "log10",
            breaks = y_breaks,
            labels = y_labels
        )

    if (annotate == "minimal") {
        p <- p + ggplot2::geom_text(
            data = annotation,
            ggplot2::aes(
                x = CoreLabel,
                y = min(data$Time, na.rm = TRUE) / annotate.position,
                label = sprintf("%.2f s", Gmean),
                group = Algorithm,
                vjust = 0.5
            ),
            position = dodge,
            size = 2.5,
            inherit.aes = FALSE
        )
    }

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
