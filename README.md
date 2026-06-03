# Plotting scripts

`mkexp.R` is the single mkexp2 entry point for this submodule:

```sh
./mkexp.R plot [--list] [--json]
./mkexp.R plot [--plot <id>]... [--threads T|NxMxT] [--output plots.pdf] <source>...
./mkexp.R stats [--results results] [--json]
```

Reusable plotting and stats code lives in `R/`. The old mkexp2 stats backend is
now `R/stats.R`, centered on `create_stats_summary(..., results_dir = "results")`.

Place standalone example `*.csv` files in `data/` and load them in
`instances.R`. Result files should include at least `Graph`, `K`, `Cut`,
`Imbalance`, and `Time`; optional columns include `Seed`, `Epsilon`, `Threads`,
`Cores`, `Failed`, and `Timeout`. Run `make example-pdf` or `make example-tex`
to generate example plots.

All plot functions accept `tex = FALSE` by default. With `tex = FALSE`, labels
are plain text such as `10^2`, `%`, and `(P)`. Pass `tex = TRUE` when rendering
through TikZ/LaTeX and you want labels such as `$10^2$`, `\\%`, or symbolic
timeout/imbalance/failure markers.

## Plot Function Reference

Most comparison plots take normalized matrix/result data frames through `...`.
The normal entry point for turning raw mkexp2 CSVs into those data frames is
`load_dataset()` from `R/common.R`.

### `create_performance_profile_plot(...)`

Performance profile over at least two algorithms. Each curve shows the fraction
of common instances solved within a ratio of the best objective.

Required columns default to `Algorithm`, `AvgCut`, `Timeout`, `Imbalanced`,
`Failed`, and primary key `Graph`, `K`. Key arguments: `segments`,
`segment.errors.width`, `label.timeout`, `label.imbalanced`, `label.failed`,
`colors`, `levels`, `axis.y.sparse_labels`, `plot.xlab`, `plot.ylab`,
`legend.title`, `tex`.

`create_performance_profile_data(...)` returns sampled profile data for callers
that want to build their own plot.

### `create_running_time_box_plot(...)`

Per-instance running-time box plot per algorithm, with optional failure,
timeout, and imbalance ticks above the log-scaled timing range.

Required columns default to `Algorithm`, `AvgTime`, `Timeout`, `Imbalanced`,
`Failed`, and primary key `Graph`, `K`. Key arguments: `exclude.imbalanced`,
`tick.timeout`, `tick.imbalanced`, `tick.failed`, `tick.errors.space_below`,
`tick.errors.space_between`, `label.timeout`, `label.imbalanced`,
`label.failed`, `colors`, `levels`, `annotate`, `annotate.position`,
`position.y`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_running_time_by_core_box_plot(...)`

Running-time distributions grouped by core count and algorithm.

Required columns default to `Algorithm`, `Cores`, `AvgTime`, `Timeout`,
`Imbalanced`, `Failed`, and primary key `Graph`, `K`, `Epsilon`, `Cores`. Key
arguments: `exclude.imbalanced`, `colors`, `levels`, `annotate`,
`annotate.position`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_relative_by_graph_grid_plot(...)`

Per-graph bar grid for relative cut or running time against a baseline.

Pass `baseline = <df>` and compared data frames through `...`. Required columns
default to `Algorithm`, `Graph`, `Cores`, `AvgCut` or `AvgTime`, and primary key
`Graph`, `K`, `Epsilon`, `Cores`. Key arguments: `metric = c("cut", "time")`,
`colors`, `levels`, `plot.xlab`, `plot.ylab`, `y.breaks`, `tex`.

### `create_speedup_plot(...)`

Speedup or slowdown profile against a baseline algorithm.

Pass `baseline = <df>` and compared data frames through `...`. Required columns
default to `Algorithm`, `AvgTime`, `Timeout`, `Failed`, and primary key `Graph`,
`K`. Key arguments: `fails.show`, `y.cap`, `colors`, `levels`,
`mode = c("speedup", "slowdown")`, `x.by`, `tex`.

### `create_core_speedup_plot(df)`

Single-algorithm speedup over larger core counts, using the smallest available
core count as the baseline.

Required columns default to `Algorithm`, `Cores`, `AvgTime`, and primary key
`Graph`, `K`, `Epsilon`. Key arguments: `colors`, `tex`.

### `create_imbalance_plot(...)`

Jitter plot of rows whose `MinImbalance` exceeds `Epsilon`.

Required columns are `Algorithm`, `MinImbalance`, and `Epsilon`. Key arguments:
`colors`, `levels`, `annotation`, `annotate.counts`, `tex`.

### `create_per_k_relcut_plot(...)`

Geometric-mean relative cut by `K` against a baseline.

Pass `baseline = <df>` and compared data frames through `...`. Required columns
include `Algorithm`, `Graph`, `K`, `AvgRealCut`, and `Failed`. Key arguments:
`partial`, `colors`, `levels`, `ks`, `tex`.

### `create_per_k_reltime_plot(...)`

Geometric-mean relative speedup or slowdown by `K` against a baseline.

Pass `baseline = <df>` and compared data frames through `...`. Required columns
include `Algorithm`, `Graph`, `K`, `AvgTime`, `Failed`, and `Timeout`. Key
arguments: `relation`, `partial`, `colors`, `levels`, `ks`, `tex`.

### `create_scalability_plot(data)`

Strong-scaling speedup curves over sequential running time for one algorithm.

Required columns include `Graph`, `K`, `Cores`, and `AvgTime` by default. Key
arguments: `column.time`, `colors`, `mean = c("Gmean", "Hmean")`, `scale_x`,
`scale_y`, `tex`.

### `create_time_per_edge_plot(...)`

Rolling time per edge over graph size.

Required columns default to `Algorithm`, `AvgTime`, `Timeout`, `Failed`, `M`,
and primary key `Graph`, `K`. Key arguments: `window_size`, `points`, `colors`,
`levels`, `tex`.

### `create_strong_scaling_time_per_edge_plot(...)`

Strong-scaling time per edge over compute nodes.

Required columns include `Algorithm`, `Nodes`, `AvgM`, `MinTime`, `AvgTime`,
`MaxTime`, `Failed`, `Timeout`, and `Imbalanced`. Key arguments: `colors`,
`levels`, `plot.title`, `plot.xlab`, `plot.ylab`, `y_labels`, `max_nodes`,
`mark_feasibility`, `tex`.

### `create_multinode_time_per_edge_plot(...)`

Time per edge over graph size for multinode experiments.

Required columns include `Algorithm`, `Graph`, `AvgM`, `MinTime`, `AvgTime`,
`MaxTime`, `Failed`, `Timeout`, and `Imbalanced`. Key arguments: `colors`,
`levels`, `plot.title`, `debug`, `y_labels`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_smallk_throughput_plot(...)`

Small-k throughput over compute nodes, grouped by average degree.

Required columns include `Algorithm`, `Nodes`, `AvgN`, `AvgM`, `MinTime`,
`AvgTime`, `MaxTime`, `Failed`, `Timeout`, and `Imbalanced`. Key arguments:
`colors`, `levels`, `plot.title`, `debug`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_smallk_multigraph_throughput_plot(...)`

Small-k throughput over compute nodes with graph-specific line types.

Required columns are the same as `create_smallk_throughput_plot()`, plus `Graph`.
Key arguments: `colors`, `levels`, `plot.title`, `debug`, `plot.xlab`,
`plot.ylab`, `mark_feasibility`, `tex`.

### `create_largek_throughput_plot(...)`

Large-k throughput over compute nodes, grouped by vertices per block.

Required columns include `Algorithm`, `Graph`, `Nodes`, `AvgN`, `AvgM`,
`MinTime`, `AvgTime`, `MaxTime`, `K`, `Failed`, `Timeout`, and `Imbalanced`.
Key arguments: `colors`, `levels`, `plot.xlab`, `plot.ylab`, `plot.title`,
`tex`.

### `create_memory_bar_plot(...)`

Grouped peak-memory bars per graph.

Required columns default to `Name`, `Algorithm`, and `AvgMaxHeap`; rows are
aligned by primary key `Graph`, `K`. Key arguments: `column.graph`,
`column.memory`, `column.algorithm`, `column.failed`, `colors`, `namer`,
`levels`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_relative_running_time_bar_plot(...)`

Grouped per-graph running-time bars relative to a baseline.

Pass `baseline = <df>` and compared data frames through `...`. Required columns
default to `Name`, `Algorithm`, `AvgTime`, `Timeout`, `Failed`, and primary key
`Graph`, `K`. Key arguments: `namer`, `colors`, `levels`, `plot.xlab`,
`plot.ylab`, `tex`.

### `create_relative_memory_plot(baseline, via_heap, via_rss)`

Relative peak-memory curves against a baseline.

Pass comparison data frames through `via_heap` and/or `via_rss` lists. Required
columns default to `Algorithm`, `AvgMaxHeap`, `AvgMaxRSS`, `Failed`, `M`, and
`PlotPerInstance`; rows are aligned by primary key `Graph`, `K`. Key arguments:
`window_size`, `namer`, `colors`, `levels`, `plot.xlab`, `plot.ylab`, `tex`.

### `create_running_time_breakdown_plot(data, cols)`

Stacked bar chart for timing-component breakdowns.

Required columns are the graph column, the timing component columns in `cols`,
and optionally a total-time column. Key arguments: `column.graph`,
`column.total_time`, `column.remaining_time`, `normalize`, `tex`.

### `create_dummy_plot(...)` and `create_standalone_legend(...)`

Legend utilities. `create_dummy_plot()` builds a plot that only carries an
algorithm legend. `create_standalone_legend()` renders that legend to TikZ. Key
arguments: `colors`, `levels`, `title`, `basename`, `nrow`, `position`,
`suffix`, `tex`.
