#!/usr/bin/env Rscript

#' Plot strong-scaling time per edge over compute nodes.
#'
#' @param ... Normalized distributed result data frames.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param plot.title Optional plot title; set to `NA` to omit.
#' @param plot.xlab,plot.ylab Axis labels; `NULL` uses defaults, `NA` omits.
#' @param y_labels `"dense"` or `"thin"` y-axis labeling.
#' @param max_nodes Maximum node count to include.
#' @param mark_feasibility If `TRUE`, mark feasible, imbalanced, and timeout
#'   rows with different shapes.
#' @param tex If `TRUE`, render math axis labels as TeX.
create_strong_scaling_time_per_edge_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    plot.xlab = NULL,
    plot.ylab = "Time per Edge [ns]",
    y_labels = "dense",
    max_nodes = 64,
    mark_feasibility = TRUE,
    tex = FALSE
) {
    if (is.null(plot.xlab)) {
        plot.xlab <- if (tex) "Compute Nodes ($P$)" else "Compute Nodes (P)"
    }
    all_dfs <- list(...)

    data <- purrr::map_dfr(all_dfs, \(df) {
        df %>% 
            dplyr::filter(!Failed | Timeout) %>% dplyr::mutate(
                MinTimePerEdge = (MinTime * 1e9) / (AvgM / 2),
                AvgTimePerEdge = (AvgTime * 1e9) / (AvgM / 2),
                MaxTimePerEdge = (MaxTime * 1e9) / (AvgM / 2),
                Feasibility = dplyr::case_when(
                    Imbalanced ~ "Imbalanced",
                    Timeout ~ "Timeout",
                    TRUE ~ "Feasible"
                )
            ) %>% 
            dplyr::filter(Nodes <= max_nodes) %>%
            dplyr::filter(!is.na(AvgTimePerEdge)) # Remove fails
    })

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    y.breaks = seq(-3, 16, by = 1)
    y.labels = plot_power_label(2, y.breaks, tex)
    if (y_labels == "thin") {
        y.labels <- ifelse(seq_along(y.breaks) %% 2 == 0, "", plot_power_label(2, y.breaks, tex))
    }

    p <- ggplot2::ggplot(
        data, 
        ggplot2::aes(
            x = Nodes,
            y = AvgTimePerEdge,
            color = Algorithm
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(1, 2, 4, 8, 16, 32, 64)
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = 2 ^ y.breaks,
            labels = y.labels
        )

    if (mark_feasibility) {
        p <- p + 
            ggplot2::geom_point(
                ggplot2::aes(shape = Feasibility),
                show.legend = c(color = FALSE)
            ) +
            ggplot_feasibility_scale
    } else {
        p <- p + ggplot2::geom_point()
    }


    if (!is.na(plot.title)) {
        p <- p + ggplot2::ggtitle(plot.title)
    }

    if (!is.na(plot.xlab)) {
        p <- p + ggplot2::xlab(plot.xlab)
    }
    if (!is.na(plot.ylab)) {
        p <- p + ggplot2::ylab(plot.ylab)
    }

    if (length(colors) > 0) {
        p <- p + ggplot2::scale_color_manual(values = colors)
    }

    return(p)
}

#' Plot multinode time per edge over graph size.
#'
#' @param ... Normalized distributed result data frames.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param plot.title Optional plot title; set to `NA` to omit.
#' @param debug If `TRUE`, print the prepared data frame.
#' @param y_labels `"dense"` or `"thin"` y-axis labeling.
#' @param plot.xlab,plot.ylab Axis labels; set to `NA` to omit.
#' @param tex If `TRUE`, render powers as TeX math labels.
create_multinode_time_per_edge_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    debug = FALSE,
    y_labels = "dense",
    plot.xlab = "Number of Edges",
    plot.ylab = "Time per Edge [ns]",
    tex = FALSE
) {
    all_dfs <- list(...)

    data <- purrr::map_dfr(all_dfs, \(df) {
        df %>% 
            dplyr::filter(!Failed | Timeout) %>% 
            dplyr::mutate(
                LogM = round(log2(AvgM / 2)),
                MinTimePerEdge = (MinTime * 1e9) / (AvgM / 2),
                AvgTimePerEdge = (AvgTime * 1e9) / (AvgM / 2),
                MaxTimePerEdge = (MaxTime * 1e9) / (AvgM / 2),
                Feasibility = dplyr::case_when(
                    Imbalanced ~ "Imbalanced",
                    Timeout ~ "Timeout",
                    TRUE ~ "Feasible"
                )
            )
    })

    if (debug) {
        print(data)
    }

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    y.breaks = seq(-16, 16, by = 1)
    y.labels = plot_power_label(2, y.breaks, tex)
    if (y_labels == "thin") {
        y.labels <- ifelse(seq_along(y.breaks) %% 2 == 0, "", plot_power_label(2, y.breaks, tex))
    }

    x.breaks = seq(32, 40, by = 1)
    x.labels = plot_power_label(2, x.breaks, tex)

    p <- ggplot2::ggplot(
        data, 
        ggplot2::aes(
            x = LogM,
            y = AvgTimePerEdge,
            color = Algorithm
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            ggplot2::aes(shape = Feasibility),
            show.legend = c(color = FALSE)
        ) +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = x.breaks,
            labels = x.labels
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = 2 ^ y.breaks,
            labels = y.labels
        ) +
        ggplot_feasibility_scale

    if (!is.na(plot.title)) {
        p <- p + ggplot2::ggtitle(plot.title)
    }

    if (!is.na(plot.xlab)) {
        p <- p + ggplot2::xlab(plot.xlab)
    }
    if (!is.na(plot.ylab)) {
        p <- p + ggplot2::ylab(plot.ylab)
    }

    if (length(colors) > 0) {
        p <- p + ggplot2::scale_color_manual(values = colors)
    }

    return(p)
}
