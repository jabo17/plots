#!/usr/bin/env Rscript
TEX <- TRUE

source("R/common.R")
source("R/performance_profile_plot.R")
source("R/running_time_box_plot.R")

source("instances.R")

example_performance_plot <- create_performance_profile_plot(
        kaminpar_fm,
        mtmetis,
        # ... add more datasets here ...
        # Remove the next line if you do not want to use custom colors
        colors = colors,
        tex = TRUE
    ) +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::xlab("Ratio") +
    ggplot2::ylab("Fraction of Instances") +
    ggplot2::theme(legend.position = "bottom")

example_running_time_box_plot <- create_running_time_box_plot(
        kaminpar_fm,
        mtmetis,
        colors = colors,
        tex = TRUE,
        annotate = "none"
    ) +
    ggplot2::theme_bw() +
    default_theme +
    ggplot2::ylab("Running Time (s)") +
    ggplot2::theme(legend.position = "none")

legend_plot <- ggpubr::get_legend(example_performance_plot, position = "bottom")
example_performance_plot <- example_performance_plot +
    ggplot2::theme(legend.position = "none")

open_tex("examples")
egg::ggarrange(
    example_performance_plot,
    example_running_time_box_plot,
    nrow = 1
)
dev_off()

open_tex("examples_legend")
print(ggpubr::as_ggplot(legend_plot))
dev_off()
