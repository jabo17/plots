#!/usr/bin/env Rscript

#' Plot relative cut by k against a baseline.
#'
#' @param ... Normalized result data frames to compare with `baseline`.
#' @param baseline Baseline data frame.
#' @param partial If `FALSE`, drop graphs where a compared algorithm failed.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param ks Optional k values to consider; defaults to baseline k values.
#' @param tex If `TRUE`, render powers as TeX math labels.
create_per_k_relcut_plot <- \(
    ...,
    baseline,
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
            dplyr::mutate(CutRatio = AvgRealCut / baseline$AvgRealCut) %>%
            dplyr::mutate(CutRatio = ifelse(CutRatio == Inf, NA, CutRatio))

        if (!partial) {
            good_graphs <- (all_data[[i]] %>% dplyr::group_by(Graph) %>% dplyr::filter(all(!Failed)))$Graph
            all_data[[i]] <- all_data[[i]] %>% dplyr::filter(Graph %in% good_graphs)
        }
    }

    data <- do.call(rbind, all_data)
    data <- data %>%
        dplyr::group_by(Algorithm, K) %>%
        dplyr::summarise(AvgCutRatio = Gmean(CutRatio, zero.propagate = TRUE, na.rm = TRUE), .groups = "drop")

    if (length(levels) > 0) {
        data$Algorithm <- data$Algorithm %>% factor(levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = K, y = AvgCutRatio, color = Algorithm)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(2^2, 2**4, 2**6, 2**11, 2**14, 2**17, 2**20),
            labels = plot_power_label(2, c(2, 4, 6, 11, 14, 17, 20), tex)
        )

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}
