#!/usr/bin/env Rscript

source("R/dummy_plot.R")

create_standalone_legend <- \(
    ..., 
    basename, 
    colors = c(), 
    levels = c(), 
    nrow = 1, 
    position = "bottom", 
    suffix = "",
    title = ggplot2::waiver() # or NULL or a string
) {
    plot <- create_dummy_plot(
        ...,
        colors = colors,
        levels = levels
    ) +
        default_theme +
        ggplot2::theme(legend.position = position) +
        ggplot2::guides(color = ggplot2::guide_legend(
            title = title, 
            nrow = nrow, 
            byrow = TRUE
        ))

    legend <- ggpubr::get_legend(plot, position = position)
    open_tikz(paste0(basename, "_legend", suffix))
    print(ggpubr::as_ggplot(legend))
    dev_off()
}

