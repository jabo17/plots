#!/usr/bin/env Rscript

create_memory_bar_plot <- function(
    ...,
    primary_key = c("Graph", "K"),
    column.graph = "Name",
    column.memory = "AvgMaxHeap",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    colors = c(),
    namer = identity,
    levels = c(),
    plot.xlab = "Graph",
    plot.ylab = "Peak Memory [TiB]"
) {
    all_datasets <- list(...)
    stopifnot(length(all_datasets) > 0)

    for (i in 1:length(all_datasets)) {
        all_datasets[[i]] <- all_datasets[[i]] %>% dplyr::arrange_at(primary_key)
    }

    data <- data.frame()

    for (i in 1:length(all_datasets)) {
        df <- all_datasets[[i]]

        df <- df %>% 
            dplyr::select(
                Graph = rlang::sym(column.graph),
                Algorithm = rlang::sym(column.algorithm),
                Memory = rlang::sym(column.memory)
            ) %>%
            dplyr::group_by(Algorithm, Graph) %>%
            dplyr::summarize(Memory = Gmean(Memory), .groups = "drop") 

        data <- rbind(data, df)
    }

    data <- data %>%
        dplyr::mutate(
            Algorithm = factor(Algorithm, levels = levels),
            Memory = ifelse(is.na(Memory), 0, Memory / 1024 / 1024)
        )

    p <- ggplot2::ggplot(data, ggplot2::aes(x = Graph, y = Memory, fill = Algorithm)) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), width = 0.8) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = ifelse(Memory == 0, "OOM ($>1.5$\\,TiB)", ""), 
                color = Algorithm
            ), 
            vjust = 0.5, 
            hjust = 0,
            position = ggplot2::position_dodge(width = 0.8),
            angle = 90,
            size = 10,
            size.unit = "pt"
        )

    if (!is.na(plot.xlab)) {
        p <- p + ggplot2::xlab(plot.xlab)
    }

    if (!is.na(plot.ylab)) {
        p <- p + ggplot2::ylab(plot.ylab)
    }

    if (length(colors) > 0) {
        p <- p + 
            ggplot2::scale_fill_manual(name = "Algorithm", values = colors) +
            ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(p)
}

