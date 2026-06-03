# Plotting scripts

The root-level scripts `mkplots.R` and `stats.R` are entry points used by
`mkexp2`. Reusable plot code lives in `R/`; example data, TeX assets, and
generated example PDFs are kept alongside the examples for standalone use.

Run `make init` to build the docker container for plotting.

Place your `*.csv` files in the `data/` folder and load them in `instances.R`.
The files must at least have the following columns: `Graph`, `K`, `Cut`, `Imbalance` and `Time`.
Other supported columns are `Seed` (for repetitions), `Epsilon` (for epsilons other than 0.03), `Threads` (for speedup plots) and `Failed` (for crashes).

Run `make example-pdf` or `make example-tex` to generate example plots with the example data provided in the `data/` folder.

Plot types included:

* `performance_profile_plot.R`: Performance profile, i.e., one curve per algorithm showing the fraction of instances (y-axis) solved within a given factor (x-axis) of the best solution.
* `imbalance_plot.R`: One box plot for each algorithm (x-axis), plotting the imbalance (y-axis) of imbalanced partitions.
* `running_time_box_plot.R`: One box plot for each algorithm (x-axis), plotting the absolute running time (y-axis) per instance. Geometric mean running time across all instances is annotated above the x-axis.
* `speedup_plot.R`: Plots one curve per algorithm, showing the relative speedup over a baseline algorithm.
  It also supports relative slowdown by setting `mode = "slowdown"`.
* `scalability_plot.R`: Plots one curve per number of threads, showing the relative speedup (y-axis) of a parallel algorithm with the given number of threads. Instances are sorted by their absolute sequential execution time (x-axis).
