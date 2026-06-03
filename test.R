#!/usr/bin/env Rscript
TEX <- FALSE

source("R/common.R")
source("R/performance_profile_plot.R")
source("R/running_time_box_plot.R")
source("R/speedup_plot.R")

source("instances.R")

always_best <- load_dataset(
    name = "Always Best", 
    filename = "tests/AlwaysBest"
)
never_best <- load_dataset(
    name = "Never Best", 
    filename = "/data/tests/NeverBest.csv"
)
sometimes_best_a <- load_dataset(
    name = "Sometimes Best A", 
    filename = "tests/SometimesBest-A"
)
sometimes_best_b <- load_dataset(
    name = "Sometimes Best B", 
    filename = "tests/SometimesBest-B"
)
always_imbalanced <- load_dataset(
    name = "Always Imbalanced", 
    filename = "tests/AlwaysImbalanced"
)

pp_default <- create_performance_profile_plot(
        always_best,
        never_best,
        sometimes_best_a,
        sometimes_best_b
    ) +
    ggplot2::labs(title = "Performance Profile") +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

pp_with_imbalanced <- create_performance_profile_plot(
        sometimes_best_a,
        sometimes_best_b,
        always_imbalanced
    ) +
    ggplot2::labs(title = "Performance Profile") +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

open_pdf("tests")
print(pp_default)
print(pp_with_imbalanced)
dev_off()
