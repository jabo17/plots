#!/usr/bin/env Rscript
create_relative_memory_plot <- \(
    baseline,
    via_heap = list(),
    via_rss = list(),
    primary_key = c("Graph", "K"),
    column.algorithm = "Algorithm",
    column.heap = "AvgMaxHeap",
    column.rss = "AvgMaxRSS",
    column.failed = "Failed",
    column.m = "M",
    column.plot_per_instance = "PlotPerInstance",
    colors = c(),
    levels = c(),
    window_size = 50,
    namer = identity,
    plot.xlab = "Number of Edges",
    plot.ylab = "Relative Peak Memory"
) {
    baseline <- baseline %>%
        dplyr::arrange_at(primary_key)

    baseline_df <- baseline %>%
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            MaxHeap = rlang::sym(column.heap),
            MaxRSS = rlang::sym(column.rss),
            M = rlang::sym(column.m),
            PlotPerInstance = rlang::sym(column.plot_per_instance),
            Failed = rlang::sym(column.failed)
        ) %>%
        dplyr::mutate(Algorithm = namer(Algorithm))

    baseline_name <- first(baseline_df$Algorithm)

    normalize_dfs <- \(dfs, baseline_memory, memory_column) purrr::map_dfr(dfs, \(df) {
        df <- df %>% dplyr::arrange_at(primary_key)
        stopifnot(df[, primary_key] == baseline[, primary_key])

        df %>% 
            dplyr::select(
                Algorithm = rlang::sym(column.algorithm),
                Memory = rlang::sym(memory_column),
                M = rlang::sym(column.m),
                PlotPerInstance = rlang::sym(column.plot_per_instance),
                Failed = rlang::sym(column.failed)
            ) %>%
            dplyr::mutate(MemoryRatio = Memory / baseline_memory) %>%
            dplyr::arrange(M) %>%
            dplyr::mutate(
                UndirectedM = M / 2,
                Ith = 1 : nrow(df),
                Curve = zoo::rollapply(
                    MemoryRatio,
                    window_size,
                    \(x) Gmean(x, na.rm = TRUE),
                    partial = TRUE,
                    align = "right"
                )
            ) %>%
            dplyr::mutate(MemoryRatio = ifelse(PlotPerInstance, MemoryRatio, NA))
    })

    data <- base::rbind(
        normalize_dfs(via_heap, baseline_df$MaxHeap, column.heap),
        normalize_dfs(via_rss, baseline_df$MaxRSS, column.rss)
    )

    baseline_color <- "black"
    if (baseline_name %in% names(colors)) {
        baseline_color <- colors[[baseline_name]]
    }

    # na.rm: 
    # - MemoryRatio: everything is NA for algorithms without per-instance dots
    plot <- ggplot2::ggplot(data, ggplot2::aes(
        x = UndirectedM, 
        y = MemoryRatio, 
        color = Algorithm
    )) +
        ggplot2::geom_point(size = 0.4, alpha = 1 / 4, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Curve), linewidth = 1.0) +
        ggplot2::geom_hline(
            linewidth = 1.0,
            yintercept = 1,
            linetype = "solid",
            color = baseline_color
        )

    if (!is.na(plot.xlab)) {
        plot <- plot + ggplot2::xlab(plot.xlab)
    }

    if (!is.na(plot.ylab)) {
        plot <- plot + ggplot2::ylab(plot.ylab)
    }

    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return (plot)
}
