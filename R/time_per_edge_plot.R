#!/usr/bin/env Rscript
create_time_per_edge_plot <- function(
    ...,
    column.time = "AvgTime",
    column.algorithm = "Algorithm",
    column.timeout = "Timeout",
    column.failed = "Failed",
    column.m = "M",
    primary_key = c("Graph", "K"),
    window_size = 50,
    points = c(),
    colors = c(),
    levels = c()) {
    #
    all_dfs <- list(...)
    if (length(all_dfs) == 0) {
        cli::cli_abort("Need at least one data frame for plotting.")
    }

    # Run some basic sanity checks against the data frames
    for (i in 1:length(all_dfs)) {
        df <- all_dfs[[i]]

        if (!(column.algorithm %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.algorithm}} missing from data frame no. {.val {i}}.")
        }

        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]
        if (!all(algorithms == algorithm)) {
            cli::cli_abort("Rows for multiple algorithms in the same data frame no. {.val {i}}: {.val {unique(algorithms)}}")
        }

        if (!(column.time %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.time}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.timeout %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.timeout}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!column.failed %in% colnames(df)) {
            cli::cli_abort("Column {.field {column.failed}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!column.m %in% colnames(df)) {
            cli::cli_abort("Column {.field {column.m}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }

        if (length(df) != length(all_dfs[[1]])) {
            cli::cli_abort("Number of rows in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows in the first data frame.")
        }
        if (!all.equal(df[, primary_key], all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
        }
        if (!all.equal(df[column.m], all_dfs[[1]][column.m])) {
            cli::cli_abort("Number of edges in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of edges in the first data frame.")
        }
    }

    data <- data.frame()
    for (df in all_dfs) {
        algorithm <- df[column.algorithm][[1]]
        show_points <- algorithm %in% points

        data <- df %>%
            dplyr::select(
                Algorithm = rlang::sym(column.algorithm),
                M = rlang::sym(column.m),
                Time = rlang::sym(column.time)
            ) %>%
            dplyr::arrange(M) %>%
            dplyr::mutate(
                UndirectedM = M / 2,
                TimePerEdgeUS = Time / (M / 2) * 1000 * 1000,
                RollingTimePerEdgeUS = zoo::rollapply(
                    TimePerEdgeUS,
                    window_size,
                    \(x) Gmean(x, na.rm = TRUE),
                    partial = TRUE,
                    align = "right"
                ),
                TimePerEdgeUS = ifelse(show_points, TimePerEdgeUS, NA)
            ) %>%
            rbind(data)
    }

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    plot <- ggplot2::ggplot(data) +
        ggplot2::geom_point(
            ggplot2::aes(
                x = UndirectedM, 
                y = TimePerEdgeUS, 
                color = Algorithm
            ), 
            size = 0.2, 
            alpha = 1 / 3, 
            na.rm = TRUE
        ) +
        ggplot2::geom_line(
            ggplot2::aes(
                x = UndirectedM, 
                y = RollingTimePerEdgeUS, 
                color = Algorithm
            ), 
            linewidth = 1.5
        )

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(plot)
}
