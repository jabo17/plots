#' Plot a stacked running-time breakdown.
#'
#' @param data Data frame with one row per graph and timing component columns.
#' @param cols Timing component column names.
#' @param column.graph Graph-name column.
#' @param column.total_time Total-time column, or `NULL` to use the sum of
#'   `cols`.
#' @param column.remaining_time Generated column for total time not covered by
#'   `cols`.
#' @param normalize If `TRUE`, plot time fractions; otherwise plot seconds.
#' @param tex Whether labels supplied by the caller are TeX labels.
create_running_time_breakdown_plot <- function(
    data,
    cols,
    column.graph = "Graph",
    column.total_time = "AvgTime",
    column.remaining_time = "AvgTimeRemaining",
    normalize = TRUE,
    tex = FALSE
) {
    # If there is no column for the total time, compute it as the sum of the columns in `cols`
    if (is.null(column.total_time)) {
        column.total_time <- ".AvgTime"
        data <- data %>% dplyr::mutate(!!column.total_time := rowSums(dplyr::accross(all_of(cols))))
    }

    # Create a column for unaccounted time (i.e., total time minus sum of `cols`)
    data <- data %>% dplyr::mutate(
        !!column.remaining_time := !!rlang::sym(column.total_time) - rowSums(dplyr::across(all_of(cols)))
    )

    # Normalize times by total time if requested
    if (normalize) {
        for (col in cols) {
            data <- data %>% dplyr::mutate(!!col := !!rlang::sym(col) / !!rlang::sym(column.total_time))
        }
        data <- data %>% dplyr::mutate(!!column.remaining_time := !!rlang::sym(column.remaining_time) / !!rlang::sym(column.total_time))
    }

    data <- data %>%
        dplyr::select(all_of(c(column.graph, column.remaining_time, cols))) %>%
        tidyr::pivot_longer(
            cols = all_of(c(column.remaining_time, cols)),
            names_to = "Metric",
            values_to = "Value"
        )

    ggplot2::ggplot(data, ggplot2::aes(x = Graph, y = Value, fill = Metric)) +
        ggplot2::geom_col(width = 0.9, position = "stack") +
        ggplot2::labs(
            x = "Graph",
            y = ifelse(normalize, "Time Fraction", "Time [s]"),
            fill = NULL
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            legend.position = "bottom",
            legend.title = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank()
        )
}
