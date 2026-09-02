#!/usr/bin/env Rscript
default_performance_profile_segment_1 <- list(
    trans = "identity",
    to = 1.1,
    width = 3,
    breaks = seq(1.0, 1.1, by = 0.01),
    labels = c("1.0", "", "", "", "", "1.05", "", "", "", "", "1.1")
)

default_performance_profile_segment_2 <- list(
    trans = "identity",
    to = 2,
    width = 2,
    breaks = c(1.25, 1.5, 1.75, 2),
    labels = c("", "1.5", "", "2")
)

default_performance_profile_segment_3 <- list(
    trans = "log10",
    to = 100,
    width = 1,
    breaks = c(10, 100),
    labels = c("10^1", "10^2")
)

default_performance_profile_segments <- list(
    default_performance_profile_segment_1,
    default_performance_profile_segment_2,
    default_performance_profile_segment_3
)

performance_profile_segments_nolog <- list(
    default_performance_profile_segment_1,
    default_performance_profile_segment_2
)

PP_RATIO_FEASIBLE <- 100000
PP_RATIO_IMBALANCED <- 1000000
PP_RATIO_TIMEOUT <- 2000000
PP_RATIO_FAILED <- 3000000

create_performance_profile_data <- function(
    ...,
    fractions = c(
        1.0, 
        1.1, 
        PP_RATIO_FEASIBLE, 
        PP_RATIO_IMBALANCED, 
        PP_RATIO_TIMEOUT,
        PP_RATIO_FAILED
    ),
    column.objective = "AvgCut",
    column.algorithm = "Algorithm",
    column.timeout = "Timeout",
    column.imbalanced = "Imbalanced",
    column.failed = "Failed",
    primary_key = c("Graph", "K")) {
    #
    all_dfs <- list(...)

    num_rows <- nrow(all_dfs[[1]])
    best <- do.call(pmin, lapply(all_dfs, \(df) df[[column.objective]]))

    data <- purrr::map_dfr(all_dfs, function(df) {
        df <- df %>%
            dplyr::mutate(Ratio = df[[column.objective]] / best) %>%
            dplyr::mutate(Ratio = case_when(
                df[[column.imbalanced]] ~ PP_RATIO_IMBALANCED,
                df[[column.timeout]] ~ PP_RATIO_TIMEOUT,
                df[[column.failed]] & !df[[column.timeout]] ~ PP_RATIO_FAILED,
                TRUE ~ Ratio
            )) %>%
            dplyr::group_by(Ratio) %>%
            dplyr::summarise(N = dplyr::n()) %>%
            dplyr::arrange(Ratio) %>%
            dplyr::mutate(
                Fraction = cumsum(N) / num_rows,
                Algorithm = first(df[[column.algorithm]]),
                Transformed = 0
            )

        # If the first lowest Ratio in a data set is larger than 1, the curve would start at
        # Fraction = 1 / n > 0. That looks ugly, so add a fake point which forces the curve to
        # go down to Fraction = 0
        row <- df %>% dplyr::slice_min(Ratio, n = 1)
        if (first(row$Ratio) > 1) {
            row$Fraction <- 0
            df <- rbind(row, df)
        }

        purrr::map_dfr(fractions, function(frac) {
            df %>% dplyr::filter(Ratio <= frac) %>%
                dplyr::filter(Ratio == max(Ratio)) %>%
                dplyr::mutate(Matched = frac) %>%
                dplyr::select(Algorithm, Fraction, Ratio = Matched)
        })
    })

    return(data)
}


create_performance_profile_plot <- \(
    ...,
    column.objective = "AvgCut",
    column.algorithm = "Algorithm",
    column.timeout = "Timeout",
    column.imbalanced = "Imbalanced",
    column.failed = "Failed",
    primary_key = c("Graph", "K"),
    segments = default_performance_profile_segments,
    segment.errors.width = 1.5,
    label.timeout = TEX_LABEL_TIMEOUT,
    label.imbalanced = TEX_LABEL_IMBALANCED,
    label.failed = TEX_LABEL_FAILED,
    colors = c(),
    levels = c(),
    axis.y.sparse_labels = TRUE,
    plot.xlab = NA,
    plot.ylab = NA,
    legend.title = ggplot2::waiver()
) {
    all_dfs <- list(...)

    if (length(all_dfs) <= 1) {
        cli::cli_abort("Need at least two data frames for plotting a performance profile.")
    }

    # Run some basic sanity checks against the data frames
    for (i in 1:length(all_dfs)) {
        # Sort the data; replace 0 by 1 to ensure that zero-cuts do not crash our code
        df <- all_dfs[[i]] %>%
            dplyr::arrange_at(primary_key) %>%
            dplyr::mutate(!!column.objective := ifelse(.data[[column.objective]] == 0, 1, .data[[column.objective]]))
        all_dfs[[i]] <- df

        if (!(column.algorithm %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.algorithm}} missing from data frame no. {.val {i}}.")
        }

        algorithms <- df %>% dplyr::pull(rlang::sym(column.algorithm))
        algorithm <- algorithms[1]
        if (!all(algorithms == algorithm)) {
            cli::cli_abort("Rows for multiple algorithms in the same data frame no. {.val {i}}: {.val {unique(algorithms)}}")
        }

        if (!(column.objective %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.objective}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.timeout %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.timeout}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!(column.imbalanced %in% colnames(df))) {
            cli::cli_abort("Column {.field {column.imbalanced}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }
        if (!column.failed %in% colnames(df)) {
            cli::cli_abort("Column {.field {column.failed}} missing from data frame no. {.val {i}} (algorithm {.val {algorithm}}).")
        }

        if (NA %in% df[[column.objective]]) {
            cli::cli_abort("Column {.field {column.objective}} of data frame no. {.val {i}} (algorithm {.val {algorithm}}) contains {.val NA} values.")
        }
        if (-Inf %in% df[[column.objective]]) {
            cli::cli_abort("Column {.field {column.objective}} of data frame no. {.val {i}} (algorithm {.val {algorithm}}) contains {.val -Inf} values.")
        }

        if (nrow(df[, primary_key]) != nrow(all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Number of rows for the primary keys in data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match the number of rows for the primary keys in the first data frame.")
        }
        if (!all.equal(df[, primary_key], all_dfs[[1]][, primary_key])) {
            cli::cli_abort("Primary keys of data frame no. {.val {i}} (algorithm {.val {algorithm}}) does not match for all rows with the primary keys of the first data frame.")
        }
    }

    num_rows <- nrow(all_dfs[[1]])
    best <- do.call(pmin, lapply(all_dfs, \(df) df[[column.objective]]))

    data <- purrr::map_dfr(all_dfs, function(df) {
        df <- df %>%
            dplyr::mutate(Ratio = df[[column.objective]] / best) %>%
            dplyr::mutate(Ratio = case_when(
                df[[column.imbalanced]] ~ PP_RATIO_IMBALANCED,
                df[[column.timeout]] ~ PP_RATIO_TIMEOUT,
                df[[column.failed]] & !df[[column.timeout]] ~ PP_RATIO_FAILED,
                TRUE ~ Ratio
            )) %>%
            dplyr::group_by(Ratio) %>%
            dplyr::summarise(N = dplyr::n()) %>%
            dplyr::arrange(Ratio) %>%
            dplyr::mutate(
                Fraction = cumsum(N) / num_rows,
                Algorithm = first(df[[column.algorithm]]),
                Transformed = 0
            )

        # If the first lowest Ratio in a data set is larger than 1, the curve would start at
        # Fraction = 1 / n > 0. That looks ugly, so add a fake point which forces the curve to
        # go down to Fraction = 0
        row <- df %>% dplyr::slice_min(Ratio, n = 1)
        if (first(row$Ratio) > 1) {
            row$Fraction <- 0
            df <- rbind(row, df)
        }

        df
    })

    # Scale ratios to respect the segments
    offset <- 0
    from <- 1
    for (segment in segments) {
        min_value <- do.call(segment$trans, list(from))
        max_value <- do.call(segment$trans, list(segment$to))
        span <- max_value - min_value

        data <- data %>%
            dplyr::mutate(Transformed = ifelse(Transformed == 0 & Ratio >= from & Ratio < segment$to, 1, Transformed)) %>%
            dplyr::mutate(Ratio = ifelse(Transformed == 1,
                offset + segment$width * (do.call(segment$trans, list(Ratio)) - min_value) / span,
                Ratio
            )) %>%
            dplyr::mutate(Transformed = ifelse(Transformed == 1, 2, Transformed))

        offset <- offset + segment$width
        from <- segment$to
    }

    # Map errors (imbalanced, timeout, failed)
    map_errors <- function(vals) {
        purrr::map_int(vals, ~ case_when(
            .x == PP_RATIO_IMBALANCED ~ 1,
            .x == PP_RATIO_TIMEOUT ~ 2,
            .x == PP_RATIO_FAILED ~ 3,
            .x == Inf ~ 3,
            TRUE ~ 0
        ))
    }

    data <- data %>%
        dplyr::mutate(Transformed = ifelse(Transformed == 0 & Ratio >= from, 1, Transformed)) %>%
        dplyr::mutate(Ratio = ifelse(Transformed == 1,
            offset + segment.errors.width * map_errors(Ratio) / 3,
            Ratio
        )) %>%
        dplyr::mutate(Transformed = ifelse(Transformed == 1, 2, Transformed))

    # Generate x axis breaks and labels
    x_breaks <- c()
    x_labels <- c()
    offset <- 0
    from <- 1.0
    for (segment in segments) {
        min_value <- do.call(segment$trans, list(from))
        max_value <- do.call(segment$trans, list(segment$to))
        span <- max_value - min_value

        x_breaks <- c(x_breaks, sapply(segment$breaks, \(v) offset + segment$width * (do.call(segment$trans, list(v)) - min_value) / span))
        x_labels <- c(x_labels, math_labels(segment$labels))

        offset <- offset + segment$width
        from <- segment$to
    }
    x_breaks <- c(x_breaks, c(
        offset + segment.errors.width * (1 / 3),
        offset + segment.errors.width * (2 / 3),
        offset + segment.errors.width
    ))
    x_labels <- c(x_labels, c(label.imbalanced, label.timeout, label.failed))

    # Draw plot
    y_labels <- if (axis.y.sparse_labels) {
        c("0.0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0")
    } else {
        seq(0.0, 1.0, by = 0.1)
    }

    if (length(levels) > 0) {
        data$Algorithm <- factor(data$Algorithm, levels = levels)
    }

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Ratio, y = Fraction, color = Algorithm)) +
        ggplot2::scale_x_continuous(expand = c(0, 0.01), breaks = x_breaks, labels = x_labels) +
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01), breaks = seq(0.0, 1.0, by = 0.1), labels = y_labels) +
        ggplot2::geom_step(linewidth = 1.5) +
        ggplot2::guides(color = ggplot2::guide_legend(title = legend.title))

    x <- 0
    for (segment in segments) {
        x <- x + segment$width
        plot <- plot + ggplot2::geom_vline(xintercept = x)
    }

    if (!is.na(plot.xlab)) {
        plot <- plot + ggplot2::xlab(plot.xlab)
    }
    if (!is.na(plot.ylab)) {
        plot <- plot + ggplot2::ylab(plot.ylab)
    }

    # Set colors
    if (length(colors) > 0) {
        plot <- plot + ggplot2::scale_color_manual(name = "Algorithm", values = colors)
    }

    return(plot)
}
