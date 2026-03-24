#!/usr/bin/env Rscript

create_per_k_reltime_plot <- \(
    ...,
    baseline,
    relation = "speedup", 
    partial = TRUE,
    colors = c(),
    levels = c(),
    ks = c()
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
            labels = c("$2^2$", "$2^4$", "$2^6$", "$2^{11}$", "$2^{14}$", "$2^{17}$", "$2^{20}$")
        ) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = c(1/2, 1, 2, 4, 8, 16, 32, 64, 128, 256),
            labels = c("$2^{-1}$", "$2^0$", "", "$2^2$", "", "$2^4$", "", "$2^6$", "", "$2^8$")
        ) +
        ggplot2::ylab(ifelse(relation == "speedup", "Relative Speedup", "Relative Slowdown"))

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}

