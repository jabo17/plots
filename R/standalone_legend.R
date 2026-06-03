#!/usr/bin/env Rscript

source("R/dummy_plot.R")

#' Render a standalone legend to a TikZ file.
#'
#' @param ... Data frames with an `Algorithm` column.
#' @param basename Output basename for the legend file.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param nrow Number of legend rows.
#' @param position Legend position used for extraction.
#' @param suffix Output filename suffix.
#' @param title Legend title, `NULL`, or `ggplot2::waiver()`.
#' @param tex If `TRUE`, pass TeX label intent through to the dummy plot.
create_standalone_legend <- \(
    ..., 
    basename, 
    colors = c(), 
    levels = c(), 
    nrow = 1, 
    position = "bottom", 
    suffix = "",
    title = ggplot2::waiver(), # or NULL or a string
    tex = FALSE
) {
    plot <- create_dummy_plot(
        ...,
        colors = colors,
        levels = levels,
        tex = tex
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
