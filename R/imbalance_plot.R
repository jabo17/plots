#!/usr/bin/env Rscript

#' Plot observed imbalances above the requested epsilon.
#'
#' @param ... Normalized result data frames.
#' @param colors Optional named algorithm colors.
#' @param levels Optional algorithm ordering.
#' @param annotation Optional annotation data frame with `Algorithm` and
#'   `Annotation` columns.
#' @param annotate.counts If `TRUE`, annotate each algorithm with its imbalanced
#'   row count.
#' @param tex If `TRUE`, use TeX percent labels; otherwise use plain labels.
create_imbalance_plot <- function(
    ...,
    colors = c(),
    levels = c(),
    annotation = data.frame(),
    annotate.counts = FALSE,
    tex = FALSE) {
    #
    data <- do.call(rbind, list(...))
    data <- data %>% dplyr::filter(MinImbalance > Epsilon)

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Algorithm, y = MinImbalance, color = Algorithm)) +
        ggplot2::geom_jitter(size = 0.4) +
        ggplot2::scale_y_continuous(
            trans = "log2",
            breaks = c(0.03, 0.1, 0.30, 1.00, 3.0),
            labels = plot_percent_label(c(3, 10, 30, 100, 300), tex),
        ) +
        ggplot2::ylab(if (tex) "Imbalance [\\%]" else "Imbalance [%]")

    if (length(colors) > 0) {
        plot <- plot + 
            ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    if (annotate.counts) {
        annotation <- data.frame()
        for (df in list(...)) {
            count <- nrow(df %>% dplyr::filter(MinImbalance > Epsilon))
            if (count > 0) {
                annotation <- annotation %>% rbind(data.frame(
                    Algorithm = dplyr::first(df$Algorithm),
                    Annotation = count
                ))
            }
        }
    }

    if (nrow(annotation) > 0) {
        plot <- plot + 
            ggplot2::geom_text(
                ggplot2::aes(
                    x = Algorithm, 
                    y = 0.02,
                    color = Algorithm, 
                    label = Annotation
                ), 
                annotation 
            )
    }

    return(plot)
}
