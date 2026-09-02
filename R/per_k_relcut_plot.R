#!/usr/bin/env Rscript

create_per_k_relcut_plot <- \(
    ...,
    baseline,
    partial = TRUE,
    colors = c(),
    levels = c(),
    ks = c(),
    column.graph = "Graph"
) {
    if (length(ks) == 0) {
        ks <- unique(baseline$K)
    }

    baseline <- baseline %>% dplyr::arrange(.data[[column.graph]], K)
    all_data <- list(...)
    excluded_rows <- list()
    for (i in 1:length(all_data)) {
        all_data[[i]] <- all_data[[i]] %>%
            dplyr::arrange(.data[[column.graph]], K) %>%
            dplyr::mutate(CutRatio = AvgRealCut / baseline$AvgRealCut) %>%
            dplyr::mutate(CutRatio = ifelse(CutRatio == Inf, NA, CutRatio))

        if (!partial) {
            excluded_rows[[i]] <- all_data[[i]] %>%
                dplyr::filter(Failed) %>%
                dplyr::mutate(.exclusion_reason = "failed")
            good_graphs <- all_data[[i]] %>%
                dplyr::group_by(.data[[column.graph]]) %>%
                dplyr::filter(all(!Failed)) %>%
                dplyr::pull(column.graph)
            all_data[[i]] <- all_data[[i]] %>%
                dplyr::filter(.data[[column.graph]] %in% good_graphs)
        }
    }

    if (!partial) {
        log_plot_exclusions(
            dplyr::bind_rows(excluded_rows),
            "per-k relative cut plot",
            c(column.graph, "K")
        )
    }

    data <- all_data %>%
        lapply(dplyr::select, Algorithm, K, CutRatio) %>%
        dplyr::bind_rows()
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
            labels = math_labels(c("2^2", "2^4", "2^6", "2^{11}", "2^{14}", "2^{17}", "2^{20}"))
        )

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}
