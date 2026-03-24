#!/usr/bin/env Rscript

create_smallk_throughput_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    debug = FALSE,
    plot.xlab = "Compute Nodes ($P$)",
    plot.ylab = "Throughput [Edges / s]"
) {
    all_dfs <- list(...)

    data <- purrr::map_dfr(all_dfs, \(df) {
        df %>% 
            dplyr::filter(!Failed | Timeout) %>%
            dplyr::mutate(
                MinEdgesPerSecond = (AvgM / 2) / MinTime,
                AvgEdgesPerSecond = (AvgM / 2) / AvgTime,
                MaxEdgesPerSecond = (AvgM / 2) / MaxTime,
                LogNPerNode = round(log2(AvgN / Nodes)),
                LogMPerNode = round(log2(AvgM / Nodes)),
                LogAvgDegree = LogMPerNode - LogNPerNode,
                AvgDegree = factor(2 ^ LogAvgDegree),
                Feasibility = dplyr::case_when(
                    Imbalanced ~ "Imbalanced",
                    Timeout ~ "Timeout",
                    TRUE ~ "Feasible"
                )
            )
    })

    if (debug) 
        data %>% print()

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    y.breaks = seq(25, 35, by = 2)
    y.labels = paste0("$2^{", y.breaks, "}$")

    p <- ggplot2::ggplot(
        data, 
        ggplot2::aes(
            x = Nodes, 
            y = AvgEdgesPerSecond, 
            color = Algorithm, 
            linetype = AvgDegree
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            ggplot2::aes(shape = Feasibility),
            show.legend = c(color = FALSE)
        ) +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(1, 2, 4, 8, 16, 32, 64, 128)
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

create_smallk_multigraph_throughput_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    debug = FALSE,
    plot.xlab = "Compute Nodes ($P$)",
    plot.ylab = "Throughput [Edges / s]",
    mark_feasibility = TRUE
) {
    all_dfs <- list(...)

    data <- purrr::map_dfr(all_dfs, \(df) {
        df %>% 
            dplyr::filter(!Failed | Timeout) %>%
            dplyr::mutate(
                MinEdgesPerSecond = (AvgM / 2) / MinTime,
                AvgEdgesPerSecond = (AvgM / 2) / AvgTime,
                MaxEdgesPerSecond = (AvgM / 2) / MaxTime,
                LogNPerNode = round(log2(AvgN / Nodes)),
                LogMPerNode = round(log2(AvgM / Nodes)),
                LogAvgDegree = LogMPerNode - LogNPerNode,
                AvgDegree = factor(2 ^ LogAvgDegree),
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

    y.breaks = seq(25, 35, by = 2)
    y.labels = paste0("$2^{", y.breaks, "}$")

    p <- ggplot2::ggplot(
        data, 
        ggplot2::aes(
            x = Nodes, 
            y = AvgEdgesPerSecond, 
            color = Algorithm, 
            linetype = Graph
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(1, 2, 4, 8, 16, 32, 64, 128)
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = 2 ^ y.breaks,
            labels = y.labels
        )

    if (mark_feasibility) {
        p <- p + 
            ggplot2::geom_point(ggplot2::aes(shape = Feasibility)) +
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


