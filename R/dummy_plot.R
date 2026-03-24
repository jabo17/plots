#!/usr/bin/env Rscript

create_dummy_plot <- \(
    ..., 
    colors = c(), 
    levels = c(),
    title = ggplot2::waiver() # can be NULL
) {
    data <- do.call(rbind, lapply(list(...), \(df) df["Algorithm"]))
    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = 0, y = 1, color = Algorithm)) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(title = title))

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(plot)
}
