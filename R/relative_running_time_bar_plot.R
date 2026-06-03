#!/usr/bin/env Rscript

#' Plot per-graph running time relative to a baseline.
#'
#' @param ... Normalized result data frames to compare with `baseline`.
#' @param baseline Baseline data frame.
#' @param primary_key Columns used to align rows.
#' @param column.graph,column.time,column.timeout,column.algorithm,column.failed
#'   Source column names.
#' @param namer Function applied to algorithm names.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param plot.xlab,plot.ylab Axis labels; set to `NA` to omit.
#' @param tex If `TRUE`, use TeX thin-space labels for baseline seconds.
create_relative_running_time_bar_plot <- function(
    ...,
    baseline,
    primary_key = c("Graph", "K"),
    column.graph = "Name",
    column.time = "AvgTime",
    column.timeout = "Timeout",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    namer = identity,
    colors = c(),
    levels = c(),
    plot.xlab = "Graph",
    plot.ylab = "Relative Running Time",
    tex = FALSE
) {
    all_datasets <- list(...)
    stopifnot(length(all_datasets) > 0)

    baseline <- baseline %>% dplyr::arrange_at(primary_key)
    for (i in 1:length(all_datasets)) {
        all_datasets[[i]] <- all_datasets[[i]] %>% dplyr::arrange_at(primary_key)
    }

    # Check for consistent data
    for (dataset in all_datasets) {
        stopifnot(dataset[, primary_key] == baseline[, primary_key])
    }

    baseline_name <- first(baseline[[column.algorithm]])

    baseline_data <- baseline %>% 
        dplyr::select(
            Graph = rlang::sym(column.graph),
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time)
        ) %>%
        dplyr::group_by(Algorithm, Graph) %>%
        dplyr::summarize(Time = Gmean(Time), .groups = "drop") %>%
        dplyr::mutate(TimeRatio = 1.0)

    data <- baseline_data


    for (i in 1:length(all_datasets)) {
        df <- all_datasets[[i]]

        df <- df %>% 
            dplyr::select(
                Graph = rlang::sym(column.graph),
                Algorithm = rlang::sym(column.algorithm),
                Time = rlang::sym(column.time)
            ) %>%
            dplyr::group_by(Algorithm, Graph) %>%
            dplyr::summarize(Time = Gmean(Time), .groups = "drop") %>%
            dplyr::mutate(TimeRatio = Time / baseline_data$Time) 

        data <- rbind(data, df)
    }

    data <- data %>%
        dplyr::mutate(
            Algorithm = factor(Algorithm, levels = levels),
            TimeRatio = ifelse(is.nan(TimeRatio), 0, TimeRatio)
        )

    baseline_color <- "black"
    if (baseline_name %in% names(colors)) {
        baseline_color <- colors[[baseline_name]]
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = Graph, y = TimeRatio, fill = Algorithm)) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), width = 0.8) +
        ggplot2::geom_text(
            ggplot2::aes(label = ifelse(TimeRatio == 0, "OOM", ""), color = Algorithm), 
            vjust = 0.5, 
            hjust = 0,
            position = ggplot2::position_dodge(width = 0.8),
            angle = 90,
            size = 10,
            size.unit = "pt"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = ifelse(Algorithm == baseline_name, if (tex) sprintf("%.0f\\,s", Time) else sprintf("%.0f s", Time), ""),
                color = Algorithm
            ),
            position = ggplot2::position_dodge(width = 0.8),
            vjust = -1, 
            hjust = 0.5,
            size = 10,
            size.unit = "pt"
        ) +
        ggplot2::geom_hline(linewidth = 1.0, yintercept = 1, linetype = "solid", color = baseline_color)

    if (!is.na(plot.xlab)) {
        p <- p + ggplot2::xlab(plot.xlab)
    }
    if (!is.na(plot.ylab)) {
        p <- p + ggplot2::ylab(plot.ylab)
    }

    if (length(colors) > 0) {
        p <- p + 
            ggplot2::scale_fill_manual(name = "Algorithm", values = colors) +
            ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(p)
}
