#!/usr/bin/env Rscript
create_speedup_plot <- \(
    ...,
    baseline,
    fails.show = TRUE,
    y.cap = NA,
    primary_key = c("Graph", "K"),
    column.time = "AvgTime",
    column.timeout = "Timeout",
    column.algorithm = "Algorithm",
    column.failed = "Failed",
    colors = c(),
    levels = c(),
    mode = c("speedup", "slowdown"),
    x.by = 100
) {
    mode <- match.arg(mode)

    dfs <- list(...)
    if (length(dfs) == 0) {
        cli::cli_abort("Need at least one data frames for plotting a slowdown profile.")
        quit()
    }

    baseline <- baseline %>% dplyr::arrange_at(primary_key)
    dfs <- purrr::map(dfs, ~ .x %>% dplyr::arrange_at(primary_key))

    purrr::walk2(dfs, seq_along(dfs), \(df, i) {
        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]

        if (nrow(df) != nrow(baseline)) {
            cli::cli_abort("Number of rows in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows in the first data frame.")
        }
        if (!all.equal(df[, primary_key], baseline[, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
        }
    })

    baseline <- baseline %>%
        dplyr::select(
            Algorithm = rlang::sym(column.algorithm),
            Time = rlang::sym(column.time),
            Timeout = rlang::sym(column.timeout)
        )
    baseline_color <- colors[[first(baseline$Algorithm)]]

    if (NA %in% baseline$Time || Inf %in% baseline$Time || 0 %in% baseline$Time) {
        cli::cli_abort("Baseline contains {.val NA}, {.val Inf} or {.val 0} values in column {.field {column.time}}, which is not allowed")
    }

    fail_markers <- data.frame(
        Algorithm = factor(),
        Ith = numeric(),
        TimeRatio = numeric()
    )
    cap_markers <- data.frame()

    data <- purrr::map_dfr(dfs, function(df) {
        df <- df %>%
            dplyr::select(
                Algorithm = rlang::sym(column.algorithm),
                Time = rlang::sym(column.time),
                Timeout = rlang::sym(column.timeout)
            ) %>%
            dplyr::mutate(TimeRatio = if (mode == "speedup") baseline$Time / Time else Time / baseline$Time) %>%
            dplyr::arrange(TimeRatio) %>%
            dplyr::mutate(Ith = dplyr::row_number()) %>%
            dplyr::filter(!is.na(TimeRatio))

        if (nrow(df) < nrow(baseline)) {
            fail_markers <<- fail_markers %>% dplyr::bind_rows(data.frame(
                Algorithm = dplyr::first(df$Algorithm),
                Ith = max(df$Ith),
                TimeRatio = max(df$TimeRatio)
            ))
        }

        if (!is.na(y.cap)) {
            len_before <- nrow(df)
            df <- df %>% dplyr::filter(TimeRatio <= y.cap)
            len_after <- nrow(df)

            fail_markers <<- fail_markers %>% dplyr::filter(TimeRatio <= y.cap)

            if (len_after < len_before) {
                cap_markers <<- cap_markers %>% dplyr::bind_rows(data.frame(
                    Algorithm = dplyr::first(df$Algorithm),
                    Ith = max(df$Ith),
                    TimeRatio = y.cap
                ))
            }
        }

        df
    })

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Ith, y = TimeRatio, color = Algorithm)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::xlab("Number of Instances") +
        ggplot2::ylab(if (mode == "speedup") "Speedup over Baseline" else "Slowdown over Baseline") +
        ggplot2::geom_hline(
            linewidth = 1.5,
            yintercept = 1,
            linetype = "solid",
            color = baseline_color
        ) +
        ggplot2::scale_x_continuous(
            breaks = c(seq(0, nrow(baseline), by = x.by))
        ) +
        ggplot2::scale_color_manual(
            name = "Algorithm",
            values = colors
        )

    if (fails.show && nrow(fail_markers) > 0) {
        plot <- plot + ggplot2::geom_point(
            data = fail_markers,
            shape = 4,
            size = 2,
            show.legend = FALSE
        )
    }
    if (!is.na(y.cap) && nrow(cap_markers) > 0) {
        plot <- plot + ggplot2::geom_point(
            data = cap_markers,
            shape = 17,
            size = 2,
            show.legend = FALSE
        )
    }

    return (plot)
}

create_core_speedup_plot <- \(
    df,
    primary_key = c("Graph", "K", "Epsilon"),
    column.time = "AvgTime",
    column.cores = "Cores",
    column.algorithm = "Algorithm",
    colors = c()
) {
    required <- c(primary_key, column.time, column.cores, column.algorithm)
    missing <- setdiff(required, colnames(df))
    if (length(missing) > 0) {
        cli::cli_abort("Columns missing for core speedup plot: {.field {missing}}")
    }

    algorithm <- dplyr::first(df[[column.algorithm]])
    data <- df %>%
        dplyr::select(
            dplyr::all_of(primary_key),
            Time = rlang::sym(column.time),
            Cores = rlang::sym(column.cores)
        ) %>%
        dplyr::filter(is.finite(Time), Time > 0, is.finite(Cores), Cores > 0)

    if (nrow(data) == 0) {
        cli::cli_abort("No positive finite running times remain for {.val {algorithm}}.")
    }

    baseline_cores <- min(data$Cores, na.rm = TRUE)
    baseline <- data %>%
        dplyr::filter(Cores == baseline_cores) %>%
        dplyr::rename(BaselineTime = Time) %>%
        dplyr::select(dplyr::all_of(primary_key), BaselineTime)

    candidates <- data %>% dplyr::filter(Cores > baseline_cores)
    if (nrow(candidates) == 0) {
        cli::cli_abort("Need at least one core count larger than baseline {.val {baseline_cores}} for {.val {algorithm}}.")
    }

    joined <- candidates %>%
        dplyr::inner_join(baseline, by = primary_key) %>%
        dplyr::mutate(Speedup = BaselineTime / Time) %>%
        dplyr::filter(is.finite(Speedup), Speedup > 0, is.finite(BaselineTime), BaselineTime > 0) %>%
        dplyr::arrange(Cores, BaselineTime)

    if (nrow(joined) == 0) {
        cli::cli_abort("No comparable rows remain for core speedup plot.")
    }

    curve <- joined %>%
        dplyr::group_by(Cores) %>%
        dplyr::arrange(BaselineTime, .by_group = TRUE) %>%
        dplyr::mutate(
            Count = dplyr::row_number(),
            GMeanSpeedup = exp(cumsum(log(Speedup)) / Count),
            CoreLabel = paste0(Cores, " cores")
        ) %>%
        dplyr::ungroup()

    core_levels <- curve %>%
        dplyr::distinct(Cores, CoreLabel) %>%
        dplyr::arrange(Cores)
    curve$CoreLabel <- factor(curve$CoreLabel, levels = core_levels$CoreLabel)

    plot <- ggplot2::ggplot(curve, ggplot2::aes(x = BaselineTime, y = GMeanSpeedup, color = CoreLabel)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = 1, linewidth = 0.5, linetype = "dashed", color = "grey40") +
        ggplot2::scale_x_log10() +
        ggplot2::xlab(paste0("Baseline running time threshold at ", baseline_cores, " core", ifelse(baseline_cores == 1, "", "s"), " [s]")) +
        ggplot2::ylab("Geometric mean speedup") +
        ggplot2::guides(color = ggplot2::guide_legend(title = "Execution"))

    if (length(colors) >= length(core_levels$CoreLabel)) {
        plot <- plot + ggplot2::scale_color_manual(values = colors[seq_along(core_levels$CoreLabel)])
    }

    plot + ggplot2::ggtitle(paste0("Speedup: ", algorithm))
}
