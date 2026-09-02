#!/usr/bin/env Rscript

create_per_k_reltime_plot <- \(
    ...,
    baseline,
    relation = "speedup", 
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
            dplyr::mutate(
                Speedup = baseline$AvgTime / AvgTime,
                Slowdown = AvgTime / baseline$AvgTime
            )

        if (!partial) {
            excluded_rows[[i]] <- all_data[[i]] %>%
                dplyr::filter(Failed & !Timeout) %>%
                dplyr::mutate(.exclusion_reason = "failed")
            good_graphs <- all_data[[i]] %>%
                dplyr::group_by(.data[[column.graph]]) %>%
                dplyr::filter(all(!Failed | Timeout)) %>%
                dplyr::pull(column.graph)
            all_data[[i]] <- all_data[[i]] %>%
                dplyr::filter(.data[[column.graph]] %in% good_graphs)
        }
    }

    if (!partial) {
        log_plot_exclusions(
            dplyr::bind_rows(excluded_rows),
            "per-k relative time plot",
            c(column.graph, "K")
        )
    }

    data <- all_data %>%
        lapply(dplyr::select, Algorithm, K, Speedup, Slowdown) %>%
        dplyr::bind_rows() %>%
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
            labels = math_labels(c("2^2", "2^4", "2^6", "2^{11}", "2^{14}", "2^{17}", "2^{20}"))
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = c(1/2, 1, 2, 4, 8, 16, 32, 64, 128, 256),
            labels = c(math_labels("2^{-1}"), math_labels("2^0"), "", math_labels("2^2"), "", math_labels("2^4"), "", math_labels("2^6"), "", math_labels("2^8"))
        ) +
        ggplot2::ylab(ifelse(relation == "speedup", "Relative Speedup", "Relative Slowdown"))

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}
