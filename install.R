#!/usr/bin/env Rscript

mkplots_pkgs <- c(
    "tidyverse",
    "plyr",
    "cli",
    "RColorBrewer"
)

legacy_pkgs <- c(
    "tidyverse",
    "egg",
    "ggpubr",
    "gridExtra",
    "plyr",
    "cli",
    "zoo",
    "RColorBrewer",
    "tikzDevice",
    "cumstats",
    "ggh4x",
    "psych",
    "mnormt",
    "cli"
)

pkgs <- if (Sys.getenv("MKEXP2_PLOTS_NATIVE", unset = "") == "1") {
    mkplots_pkgs
} else {
    legacy_pkgs
}

repos <- getOption("repos")
if (is.null(repos) || is.na(repos[["CRAN"]]) || repos[["CRAN"]] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
}

need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need) > 0) {
    install.packages(need)
}
