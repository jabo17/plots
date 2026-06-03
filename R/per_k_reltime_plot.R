#!/usr/bin/env Rscript

#' Plot relative running time by k against a baseline.
#'
#' @param ... Normalized result data frames to compare with `baseline`.
#' @param baseline Baseline data frame.
#' @param relation Either `"speedup"` or `"slowdown"`.
#' @param partial If `FALSE`, drop graphs where a compared algorithm failed.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param ks Optional k values to consider; defaults to baseline k values.
#' @param tex If `TRUE`, render powers as TeX math labels.
create_per_k_reltime_plot <- \(
    ...,
    baseline,
    relation = "speedup", 
    partial = TRUE,
    colors = c(),
    levels = c(),
    ks = c(),
    tex = FALSE
) {
    if (length(ks) == 0) {
        ks <- unique(baseline$K)
    }

    baseline <- baseline %>% dplyr::arrange(Graph, K)
    all_data <- list(...)
    for (i in 1:length(all_data)) {
        all_data[[i]] <- all_data[[i]] %>%
            dplyr::arrange(Graph, K) %>%
            dplyr::mutate(
                Speedup = baseline$AvgTime / AvgTime,
                Slowdown = AvgTime / baseline$AvgTime
            )

        if (!partial) {
            good_graphs <- (all_data[[i]] %>% dplyr::group_by(Graph) %>% dplyr::filter(all(!Failed | Timeout)))$Graph
            all_data[[i]] <- all_data[[i]] %>% dplyr::filter(Graph %in% good_graphs)
        }
    }

    data <- do.call(rbind, all_data) %>% 
        dplyr::group_by(Algorithm, K) %>%
        dplyr::summarize(
            Reltime = ifelse(
                relation == "speedup", 
                Gmean(Speedup, zero.propagate = TRUE, na.rm = TRUE), 
                Gmean(Slowdown, zero.propagate = TRUE, na.rm = TRUE)
            ),
            .groups = "drop"
        )

    if (length(levels) > 0) {
        data$Algorithm <- data$Algorithm %>% factor(levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = K, y = Reltime, color = Algorithm)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(2^2, 2**4, 2**6, 2**11, 2**14, 2**17, 2**20),
            labels = plot_power_label(2, c(2, 4, 6, 11, 14, 17, 20), tex)
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = c(1/2, 1, 2, 4, 8, 16, 32, 64, 128, 256),
            labels = c(plot_power_label(2, -1, tex), plot_power_label(2, 0, tex), "", plot_power_label(2, 2, tex), "", plot_power_label(2, 4, tex), "", plot_power_label(2, 6, tex), "", plot_power_label(2, 8, tex))
        ) +
        ggplot2::ylab(ifelse(relation == "speedup", "Relative Speedup", "Relative Slowdown"))

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}
