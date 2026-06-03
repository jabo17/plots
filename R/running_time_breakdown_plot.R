###
# Creates a stacked bar plot showing the breakdown of total partitioning time into different components.
#
# `data`: A data frame, where each row corresponds to a graph, and contains columns for:
# - the graph name (not shown anywhere)
# - the total partitioning time (if `column.total_time` is NULL, this is computed as the sum of the columns in `cols`)
# - the time spent in each component listed in `cols`
#
# `cols`: A character vector of column names in `data` that contain the time spent in each component.
#
# `column.graph`: The name of the column in `data` that contains the graph names (default: "Graph").
#
# `column.total_time`: The name of the column in `data` that contains the total partitioning time (default: "AvgTime").
# If NULL, the total time is computed as the sum of the columns in `cols`.
#
# `column.remaining_time`: The name of the column to create that contains the remaining time (
# total time minus sum of `cols`) (default: "AvgTimeRemaining").
#
# `normalize`: If TRUE, the times are normalized by the total time to show fractions (default: TRUE).
# If FALSE, absolute times are shown.
###
create_running_time_breakdown_plot <- function(
    data,
    cols,
    column.graph = "Graph",
    column.total_time = "AvgTime",
    column.remaining_time = "AvgTimeRemaining",
    normalize = TRUE
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
