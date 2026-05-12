#!/usr/bin/env Rscript

relative_by_graph_grid_theme <- \() {
    ggplot2::theme(
        aspect.ratio = 1,
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 6),
        strip.text = ggplot2::element_text(size = 6),
        panel.spacing = ggplot2::unit(0.25, "lines")
    )
}

create_relative_by_graph_grid_plot <- \(
    ...,
    baseline,
    metric = c("cut", "time"),
    primary_key = c("Graph", "K", "Epsilon", "Cores"),
    column.algorithm = "Algorithm",
    column.graph = "Graph",
    column.cores = "Cores",
    column.cut = "AvgCut",
    column.time = "AvgTime",
    colors = c(),
    levels = c(),
    plot.xlab = "Cores",
    plot.ylab = NULL
) {
    metric <- match.arg(metric)
    column.value <- if (metric == "cut") column.cut else column.time
    metric_label <- if (metric == "cut") "Relative Cut" else "Relative Running Time"
    if (is.null(plot.ylab)) {
        plot.ylab <- metric_label
    }

    datasets <- c(list(baseline), list(...))
    if (length(datasets) < 1) {
        cli::cli_abort("Need at least one data frame for plotting a relative graph grid.")
    }

    required <- unique(c(primary_key, column.algorithm, column.graph, column.cores, column.value))
    for (i in 1 : length(datasets)) {
        missing <- setdiff(required, colnames(datasets[[i]]))
        if (length(missing) > 0) {
            algorithm <- if (column.algorithm %in% colnames(datasets[[i]])) {
                datasets[[i]][[column.algorithm]][1]
            } else {
                paste0("data frame no. ", i)
            }
            cli::cli_abort(
                "Columns missing from {.val {algorithm}} for relative graph grid plot: {.field {missing}}"
            )
        }
    }

    baseline_name <- baseline[[column.algorithm]][1]

    normalize <- \(df) {
        selected <- df %>% dplyr::select(dplyr::all_of(required))
        selected$Algorithm <- selected[[column.algorithm]]
        selected$Graph <- selected[[column.graph]]
        selected$Cores <- selected[[column.cores]]
        selected$Value <- selected[[column.value]]
        selected %>%
            dplyr::filter(is.finite(Value), Value > 0)
    }

    baseline_data <- normalize(baseline) %>%
        dplyr::select(dplyr::all_of(primary_key), BaselineValue = Value)

    data <- purrr::map_dfr(datasets, normalize) %>%
        dplyr::inner_join(baseline_data, by = primary_key) %>%
        dplyr::mutate(
            Ratio = Value / BaselineValue,
            Algorithm = if (length(levels) > 0) {
                factor(Algorithm, levels = levels, ordered = TRUE)
            } else {
                factor(Algorithm, levels = unique(Algorithm), ordered = TRUE)
            },
            Cores = as.integer(Cores)
        ) %>%
        dplyr::filter(is.finite(Ratio), Ratio > 0) %>%
        dplyr::group_by(Graph, Algorithm, Cores) %>%
        dplyr::summarize(Ratio = Gmean(Ratio), .groups = "drop") %>%
        dplyr::mutate(
            CoreLabel = factor(Cores, levels = sort(unique(Cores)))
        )

    if (nrow(data) == 0) {
        cli::cli_abort("No comparable data remains for the {.val {metric_label}} graph grid plot.")
    }

    num_graphs <- length(unique(data$Graph))
    num_cols <- ceiling(sqrt(num_graphs))
    baseline_color <- if (baseline_name %in% names(colors)) colors[[baseline_name]] else "black"

    p <- ggplot2::ggplot(data, ggplot2::aes(x = CoreLabel, y = Ratio, fill = Algorithm)) +
        ggplot2::geom_col(
            position = ggplot2::position_dodge(width = 0.8),
            width = 0.7
        ) +
        ggplot2::geom_hline(yintercept = 1.0, linewidth = 0.3, color = baseline_color) +
        ggplot2::facet_wrap(~ Graph, ncol = num_cols) +
        ggplot2::ylab(plot.ylab) +
        ggplot2::xlab(plot.xlab)

    if (length(colors) > 0) {
        p <- p + ggplot2::scale_fill_manual(name = "Algorithm", values = colors)
    }

    p
}
