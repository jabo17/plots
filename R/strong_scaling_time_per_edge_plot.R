#!/usr/bin/env Rscript

create_strong_scaling_time_per_edge_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    plot.xlab = "Compute Nodes ($P$)",
    plot.ylab = "Time per Edge [ns]",
    y_labels = "dense",
    max_nodes = 64,
    mark_feasibility = TRUE
) {
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
    y.labels = paste0("$2^{", y.breaks, "}$")
    if (y_labels == "thin") {
        y.labels <- ifelse(seq_along(y.breaks) %% 2 == 0, "", paste0("$2^{", y.breaks, "}$"))
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

create_multinode_time_per_edge_plot <- \(
    ...,
    colors = c(),
    levels = c(),
    plot.title = NA,
    debug = FALSE,
    y_labels = "dense",
    plot.xlab = "Number of Edges",
    plot.ylab = "Time per Edge [ns]"
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
    y.labels = paste0("$2^{", y.breaks, "}$")
    if (y_labels == "thin") {
        y.labels <- ifelse(seq_along(y.breaks) %% 2 == 0, "", paste0("$2^{", y.breaks, "}$"))
    }

    x.breaks = seq(32, 40, by = 1)
    x.labels = paste0("$2^{", x.breaks, "}$")

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

