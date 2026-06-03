#!/usr/bin/env Rscript

#' Plot large-k throughput over compute nodes.
#'
#' @param ... Normalized distributed result data frames.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param plot.xlab,plot.ylab Axis labels; `NULL` uses defaults, `NA` omits.
#' @param plot.title Optional plot title; set to `NA` to omit.
#' @param tex If `TRUE`, render math axis labels as TeX.
create_largek_throughput_plot <- \(
    ..., 
    colors = c(),
    levels = c(),
    plot.xlab = NULL,
    plot.ylab = "Throughput [Edges / s]",
    plot.title = NA,
    tex = FALSE
) {
    if (is.null(plot.xlab)) {
        plot.xlab <- if (tex) "Compute Nodes ($P$)" else "Compute Nodes (P)"
    }
    all_dfs <- list(...)

    data <- purrr::map_dfr(all_dfs, \(df) {
        df %>% dplyr::filter(!Failed | Timeout) %>% dplyr::mutate(
            MinEdgesPerSecond = (AvgM / 2) / MinTime,
            AvgEdgesPerSecond = (AvgM / 2) / AvgTime,
            MaxEdgesPerSecond = (AvgM / 2) / MaxTime,
            Feasibility = dplyr::case_when(
                Imbalanced ~ "Imbalanced",
                Timeout ~ "Timeout",
                TRUE ~ "Feasible"
            )
        )
    })

    # rgg2d-... -> rgg2d
    # rgg3d-... -> rgg3d
    # rhg-... -> rhg
    data <- data %>% 
        dplyr::mutate(
            SimpleGraph = stringr::str_extract(Graph, "^[^-]+"),
            LogVerticesPerBlock = factor(round(log2(AvgN / K)))
        )

    y.breaks = seq(20, 35, by = 2)
    y.labels = plot_power_label(2, y.breaks, tex)

    p <- ggplot2::ggplot(
        data, 
        ggplot2::aes(
            x = Nodes,
            y = AvgEdgesPerSecond,
            color = Algorithm,
            linetype = LogVerticesPerBlock
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(ggplot2::aes(shape = Feasibility)) +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(1, 2, 4, 8, 16, 32, 64, 128)
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = 2 ^ y.breaks,
            labels = y.labels
        ) +
        ggplot2::scale_shape_manual(
            values = c(0, 2, 3),
            name = "Feasibility"
        )

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
