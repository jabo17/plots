#!/usr/bin/env Rscript
source("R/common.R")
source("R/running_time_breakdown_plot.R")

# The default aggregator only aggregates over standard columns like Cut, Time, etc.
# Extend to for the additional timing columns:
aggreate_running_times <- \(df) {
    df %>% default_aggregator() %>% dplyr::mutate(
        AvgCoarseningTime = mean(df$TimeCoarsening),
        AvgInitialPartitioningTime = mean(df$TimeInitialPartitioning),
        AvgUncoarseningTime = mean(df$TimeUncoarsening),
        AvgRefinementTime = mean(df$TimeRefinement)
    )
}

kaminpar <- load_dataset(
    name = "KaMinPar",
    filename = "KaMinPar-Timings",
    aggregator = aggreate_running_times
)

# Plot absolute running times
example_normalized_breakdown_plot <- create_running_time_breakdown_plot(
    data = kaminpar %>% dplyr::filter(K == 16), # only one value of K
    cols = c(
        "AvgCoarseningTime", 
        "AvgInitialPartitioningTime", 
        "AvgUncoarseningTime", 
        "AvgRefinementTime"
    )
)

# Plot fractions of the per-instance running times
example_absolute_breakdown_plot <- create_running_time_breakdown_plot(
    data = kaminpar %>% dplyr::filter(K == 16), # only one value of K
    cols = c(
        "AvgCoarseningTime", 
        "AvgInitialPartitioningTime", 
        "AvgUncoarseningTime", 
        "AvgRefinementTime"
    ),
    normalize = FALSE
)

open_pdf("breakdown")
print(example_normalized_breakdown_plot)
print(example_absolute_breakdown_plot)
dev_off()
