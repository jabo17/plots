#!/usr/bin/env Rscript
TEX <- FALSE

source("R/common.R")
source("R/performance_profile_plot.R")
source("R/running_time_box_plot.R")
source("R/speedup_plot.R")

source("instances.R")

example_performance_plot <- create_performance_profile_plot(
        kaminpar_fm,
        mtmetis,
        # ... add more datasets here ...
        # Remove the next line if you do not want to use custom colors
        colors = colors
    ) +
    ggplot2::labs(title = "Performance Profile") +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

example_running_time_box_plot <- create_running_time_box_plot(
        kaminpar_fm,
        mtmetis,
        colors = colors
    ) +
    ggplot2::labs(
        title = "Per-Instance Running Times",
        subtitle = "With geometric mean running time per algorithm"
    ) +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::ylab("Running Time (s)") +
    ggplot2::theme(legend.position = "bottom")

example_slowdown_plot <- create_speedup_plot(
        kaminpar_fm, mtmetis,
        baseline = kaminpar_fm,
        colors = colors,
        mode = "slowdown"
    ) +
    ggplot2::labs(
        title = "Running Time Relative to Baseline",
        subtitle = "< 1 is faster"
    ) +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::theme(legend.position = "bottom")

example_speedup_plot <- create_speedup_plot(
        kaminpar_fm, mtmetis,
        baseline = kaminpar_fm,
        colors = colors,
        mode = "speedup"
    ) +
    ggplot2::labs(
        title = "Running Time Relative to Baseline",
        subtitle = "> 1 is faster"
    ) +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::theme(legend.position = "bottom")

open_pdf("examples")
print(example_performance_plot)
print(example_running_time_box_plot)
print(example_slowdown_plot)
print(example_speedup_plot)
dev_off()
