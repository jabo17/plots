#!/usr/bin/env Rscript

create_scalability_plot <- function(
    data,
    column.time = "AvgTime",
    colors = c(),
    mean = "Gmean", # Gmean or Hmean
    scale_x = TRUE,
    scale_y = TRUE) {
    #
    core_counts <- unique(data$Cores)
    graphs <- unique(data$Graph)

    trans <- data.frame(
        Graph = factor(),
        K = factor(),
        Cores = factor(),
        SequentialTime = numeric(),
        Time = numeric(),
        Speedup = factor()
    )

    for (graph in graphs) {
        this <- data %>%
            dplyr::filter(Graph == graph) %>%
            dplyr::arrange(K)
        sequential <- this %>% dplyr::filter(Cores == 1)

        for (cores in core_counts) {
            if (cores == 1) {
                next
            }

            time <- this %>%
                dplyr::filter(Cores == cores) %>%
                dplyr::mutate(
                    SequentialTime = sequential[[column.time]],
                    Time = .data[[column.time]],
                    Speedup = sequential[[column.time]] / Time 
                ) %>%
                dplyr::select(
                    Graph,
                    K,
                    Cores,
                    SequentialTime,
                    Time,
                    Speedup
                )
            trans <- rbind(trans, time)
        }
    }

    trans <- trans %>%
        dplyr::group_by(Cores) %>%
        dplyr::arrange(desc(SequentialTime)) %>%
        dplyr::mutate(
            CumulativeSpeedup = cumstats::cumhmean(Speedup),
            CumulativeSpeedupGM = cumstats::cumgmean(Speedup)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            Cores = factor(Cores, levels = sort(unique(as.integer(Cores))))
        )

    #print(column.time)
    #trans %>%
    #    dplyr::filter(Cores == 64) %>%
    #    dplyr::arrange(Speedup) %>%
    #    print(n = 5)

    plot <- ggplot2::ggplot(trans, ggplot2::aes(color = Cores, y = Speedup, x = SequentialTime)) +
        ggplot2::geom_point(size = 0.5)
    if (mean == "Gmean") {
        plot <- plot + ggplot2::geom_line(ggplot2::aes(x = SequentialTime, y = CumulativeSpeedupGM), linewidth = 1.5)
    } else if (mean == "Hmean") {
        plot <- plot + ggplot2::geom_line(ggplot2::aes(x = SequentialTime, y = CumulativeSpeedup), linewidth = 1.5)

    }

    plot <- plot + ggplot2::xlab("Single-Threaded Running Time [s]") +
        ggplot2::ylab("Speedup") +
        ggplot2::expand_limits(y = c(4, 64))

    if (scale_y) {
        plot <- plot + ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = c(4, 8, 16, 32, 64),
            labels = c("4", "8", "16", "32", "64")
        )
    }
    if (scale_x) {
        plot <- plot + ggplot2::scale_x_continuous(
            trans = "log2",
            breaks = c(1, 4, 16, 64, 256, 1024, 4096),
            labels = c("1", "4", "16", "64", "256", "1024", "4096")
        )
    }

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(values = colors)
    }

    return(plot)
}
