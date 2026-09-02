#!/usr/bin/env Rscript
options(tidyverse.quiet = TRUE)

library(tidyverse, warn.conflicts = FALSE)
library(cli, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(tikzDevice, warn.conflicts = FALSE)
library(cumstats, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(ggpubr, warn.conflicts = FALSE)
library(grid, warn.conflicts = FALSE)

DATA_INPUT_DIR <- "/data"
CACHE_DIR <- "/cache"

TEX_CLASS <- "/doc/lipics-v2021"
TEX_INPUT <- "/doc/definitions.tex"

TEX_OUTPUT <- "/plots"
DATA_OUTPUT <- "/data"
PDF_OUTPUT <- "."

PDF_LABEL_TIMEOUT <- "T"
PDF_LABEL_IMBALANCED <- "I"
PDF_LABEL_FAILED <- "F"

TEX_LABEL_TIMEOUT <- "\\SymbTimeout"
TEX_LABEL_IMBALANCED <- "\\SymbImbalanced"
TEX_LABEL_FAILED <- "\\SymbFailed"

DEFAULT_TIMELIMIT <- 90 * 60 
DEFAULT_EPSILON <- 0.03

PERFORMANCE_PROFILE_XLAB <- "Ratio"
PERFORMANCE_PROFILE_YLAB <- "Fraction of Instances"

scalability_aggregator <- function(df) {
    data.frame(
        MinRealCut = ifelse(all(is.na(df$RealCut)), NA, min(df$RealCut, na.rm = TRUE)),
        AvgRealCut = ifelse(all(is.na(df$RealCut)), NA, mean(df$RealCut, na.rm = TRUE)),
        MaxRealCut = ifelse(all(is.na(df$RealCut)), NA, max(df$RealCut, na.rm = TRUE)),
        MinCut = ifelse(all(is.na(df$Cut)), NA, min(df$Cut, na.rm = TRUE)),
        AvgCut = ifelse(all(is.na(df$Cut)), NA, mean(df$Cut, na.rm = TRUE)),
        MaxCut = ifelse(all(is.na(df$Cut)), NA, max(df$Cut, na.rm = TRUE)),
        MinImbalance = ifelse(all(is.na(df$Imbalance)), NA, min(df$Imbalance, na.rm = TRUE)),
        AvgImbalance = ifelse(all(is.na(df$Imbalance)), NA, mean(df$Imbalance, na.rm = TRUE)),
        MaxImbalance = ifelse(all(is.na(df$Imbalance)), NA, max(df$Imbalance, na.rm = TRUE)),
        MinTime = ifelse(all(is.na(df$Time)), NA, min(df$Time, na.rm = TRUE)),
        AvgTime = ifelse(all(is.na(df$Time)), NA, mean(df$Time, na.rm = TRUE)),
        MaxTime = ifelse(all(is.na(df$Time)), NA, max(df$Time, na.rm = TRUE)),
        AvgTimeCoarsening = ifelse(all(is.na(df$TimeCoarsening)), NA, mean(df$TimeCoarsening, na.rm = TRUE)),
        AvgTimeRefinement = ifelse(all(is.na(df$TimeRefinement)), NA, mean(df$TimeRefinement, na.rm = TRUE)),
        AvgTimeUncoarsening = ifelse(all(is.na(df$TimeUncoarsening)), NA, mean(df$TimeUncoarsening, na.rm = TRUE)),
        AvgTimeInitialPartitioning = ifelse(all(is.na(df$TimeInitialPartitioning)), NA, mean(df$TimeInitialPartitioning, na.rm = TRUE)),
        Timeout = all(df$Timeout),
        Failed = all(df$Failed)
    )
}

default_aggregator <- function(df) {
    data.frame(
        MinRealCut = ifelse(all(is.na(df$RealCut)), NA, min(df$RealCut, na.rm = TRUE)),
        AvgRealCut = ifelse(all(is.na(df$RealCut)), NA, mean(df$RealCut, na.rm = TRUE)),
        MaxRealCut = ifelse(all(is.na(df$RealCut)), NA, max(df$RealCut, na.rm = TRUE)),
        MinCut = ifelse(all(is.na(df$Cut)), NA, min(df$Cut, na.rm = TRUE)),
        AvgCut = ifelse(all(is.na(df$Cut)), NA, mean(df$Cut, na.rm = TRUE)),
        MaxCut = ifelse(all(is.na(df$Cut)), NA, max(df$Cut, na.rm = TRUE)),
        MinImbalance = ifelse(all(is.na(df$Imbalance)), NA, min(df$Imbalance, na.rm = TRUE)),
        AvgImbalance = ifelse(all(is.na(df$Imbalance)), NA, mean(df$Imbalance, na.rm = TRUE)),
        MaxImbalance = ifelse(all(is.na(df$Imbalance)), NA, max(df$Imbalance, na.rm = TRUE)),
        MinTime = ifelse(all(is.na(df$Time)), NA, min(df$Time, na.rm = TRUE)),
        AvgTime = ifelse(all(is.na(df$Time)), NA, mean(df$Time, na.rm = TRUE)),
        MaxTime = ifelse(all(is.na(df$Time)), NA, max(df$Time, na.rm = TRUE)),
        Timeout = all(df$Timeout),
        Failed = all(df$Failed)
    )
}

default_hypergraph_aggregator <- function(df) {
    df %>% default_aggregator %>% dplyr::mutate(
        MinRealKM1 = ifelse(all(is.na(df$RealKM1)), NA, min(df$RealKM1, na.rm = TRUE)),
        AvgRealKM1 = ifelse(all(is.na(df$RealKM1)), NA, mean(df$RealKM1, na.rm = TRUE)),
        MaxRealKM1 = ifelse(all(is.na(df$RealKM1)), NA, max(df$RealKM1, na.rm = TRUE)),
        MinKM1 = ifelse(all(is.na(df$KM1)), NA, min(df$KM1, na.rm = TRUE)),
        AvgKM1 = ifelse(all(is.na(df$KM1)), NA, mean(df$KM1, na.rm = TRUE)),
        MaxKM1 = ifelse(all(is.na(df$KM1)), NA, max(df$KM1, na.rm = TRUE)),
        MinRealObjVal = ifelse(all(is.na(df$RealObjVal)), NA, min(df$RealObjVal, na.rm = TRUE)),
        AvgRealObjVal = ifelse(all(is.na(df$RealObjVal)), NA, mean(df$RealObjVal, na.rm = TRUE)),
        MaxRealObjVal = ifelse(all(is.na(df$RealObjVal)), NA, max(df$RealObjVal, na.rm = TRUE)),
        MinObjVal = ifelse(all(is.na(df$ObjVal)), NA, min(df$ObjVal, na.rm = TRUE)),
        AvgObjVal = ifelse(all(is.na(df$ObjVal)), NA, mean(df$ObjVal, na.rm = TRUE)),
        MaxObjVal = ifelse(all(is.na(df$ObjVal)), NA, max(df$ObjVal, na.rm = TRUE)),
    )
}

mem_normalizer <- \(df, aggregator = mem_aggregator, timelimit = DEFAULT_TIMELIMIT) {
    if (!("MaxRSS" %in% colnames(df))) {
        df$MaxRSS <- -1
    }
    if (!("MaxHeap" %in% colnames(df))) {
        if ("PeakMemory" %in% colnames(df)) {
            df$MaxHeap <- df$PeakMemory
        } else {
            df$MaxHeap <- -1
        }
    }

    df %>% 
        dplyr::mutate(
            MaxRSS = ifelse(Failed == "1", NA, MaxRSS),
            MaxHeap = ifelse(Failed == "1", NA, MaxHeap)
        ) %>%
        default_normalizer(
            aggregator = aggregator,
            timelimit = timelimit
        )
}

mem_aggregator <- \(df) {
    df %>% default_aggregator() %>% dplyr::mutate(
        AvgMaxRSS = ifelse(all(is.na(df$MaxRSS)), NA, mean(df$MaxRSS, na.rm = TRUE)),
        AvgMaxHeap = ifelse(all(is.na(df$MaxHeap)), NA, mean(df$MaxHeap, na.rm = TRUE))
    )
}

treat_imbalanced_as_balanced <- \(df) df %>%
    dplyr::mutate(
        MinCut = MinRealCut,
        AvgCut = AvgRealCut,
        MaxCut = MaxRealCut,
        Imbalanced = FALSE,
        Infeasible = FALSE,
        Feasible = !Failed & !Timeout
    )

hierarchy_level_aggregator <- function(df) {
    default_aggregator(df) %>% dplyr::mutate(
        AvgHierarchyLevels = mean(df$HierarchyLevels, na.rm = TRUE),
        AvgCoarsestN = mean(df$CoarsestN, na.rm = TRUE)
    )
}

DEFAULT_BY <- c("Algorithm", "Graph", "K", "Epsilon", "Cores")

DEFAULT_HYPERGRAPH_BY <- c("Algorithm", "Hypergraph", "K", "Epsilon", "Cores")

dist_aggregator <- \(df) {
    df %>% default_aggregator %>% dplyr::mutate(
        AvgN = mean(df$N, na.rm = TRUE),
        AvgM = mean(df$M, na.rm = TRUE)
    )
}

dist_hypergraph_aggregator <- \(df) {
    df %>% default_hypergraph_aggregator %>% dplyr::mutate(
        AvgN = mean(df$N, na.rm = TRUE),
        AvgM = mean(df$M, na.rm = TRUE),
        AvgPins = mean(df$Pins, na.rm = TRUE)
    )
}

dist_normalizer <- \(
    df,
    aggregator = default_aggregator,
    timelimit = DEFAULT_TIMELIMIT
) {
    if (!("Nodes" %in% colnames(df)) && "NumNodes" %in% colnames(df)) {
        df$Nodes <- df$NumNodes
    }

    default_normalizer(
        df, 
        aggregator = aggregator, 
        timelimit = timelimit, 
        by = c(DEFAULT_BY, "Nodes"),
        aggregate.pre = \(df) df %>% dplyr::mutate(
            N = ifelse(Failed, NA, N),
            M = ifelse(Failed, NA, M)
        )
    )
}

default_normalizer <- \(
    df, 
    aggregator = default_aggregator, 
    timelimit = DEFAULT_TIMELIMIT, 
    by = DEFAULT_BY,
    aggregate.pre = identity,
    aggregate.post = identity
) {
    df %>%
        dplyr::mutate(Graph = sub("\\.metis|\\.bgf|\\.mtx|\\.mtx.hgr|\\.hgr|\\.graph|\\.scotch", "", Graph)) %>%
        dplyr::mutate(Failed = ifelse(Failed == "1", TRUE, FALSE)) %>%
        dplyr::mutate(Timeout = ifelse(Timeout == "1", TRUE, FALSE)) %>%
        dplyr::mutate(Imbalance = ifelse(Failed | Timeout, NA, Imbalance)) %>%
        dplyr::mutate(Time = ifelse(Timeout, timelimit, ifelse(Failed, NA, Time))) %>%
        dplyr::mutate(RealCut = ifelse(Failed | Timeout, NA, Cut)) %>%
        dplyr::mutate(Cut = ifelse(Failed | Timeout | (Imbalance > Epsilon + .Machine$double.eps), NA, Cut)) %>%
        aggregate.pre() %>%
        plyr::ddply(by, aggregator) %>%
        aggregate.post() %>%
        dplyr::mutate(AvgCut = ifelse(is.na(AvgCut), Inf, AvgCut)) %>%
        dplyr::mutate(AvgRealCut = ifelse(is.na(AvgRealCut), Inf, AvgRealCut)) %>%
        dplyr::mutate(Infeasible = !Failed & !Timeout & (AvgCut == Inf)) %>%
        dplyr::mutate(Imbalanced = !Failed & !Timeout & (AvgCut == Inf)) %>%
        dplyr::mutate(Feasible = !Failed & !Timeout & !Infeasible)
}

default_hypergraph_normalizer <- \(
    df, 
    aggregator = default_hypergraph_aggregator, 
    timelimit = DEFAULT_TIMELIMIT, 
    by = DEFAULT_HYPERGRAPH_BY,
    aggregate.pre = identity,
    aggregate.post = identity
) {
    df %>%
        dplyr::mutate(Hypergraph = sub("\\.zoltan.hg|\\.metis|\\.bgf|\\.mtx|\\.mtx.hgr|\\.hgr|\\.graph|\\.scotch", "", Hypergraph)) %>%
        dplyr::mutate(Failed = ifelse(Failed == "1", TRUE, FALSE)) %>%
        dplyr::mutate(Timeout = ifelse(Timeout == "1", TRUE, FALSE)) %>%
        dplyr::mutate(Imbalance = ifelse(Failed | Timeout, NA, Imbalance)) %>%
        dplyr::mutate(Time = ifelse(Timeout, timelimit, ifelse(Failed, NA, Time))) %>%
        dplyr::mutate(RealCut = ifelse(Failed | Timeout, NA, Cut)) %>%
        dplyr::mutate(Cut = ifelse(Failed | Timeout | (Imbalance > Epsilon + .Machine$double.eps), NA, Cut)) %>%
        dplyr::mutate(RealKM1 = ifelse(Failed | Timeout, NA, Cut)) %>%
        dplyr::mutate(KM1 = ifelse(Failed | Timeout | (Imbalance > Epsilon + .Machine$double.eps), NA, KM1)) %>%
        # set ObjVal to cut or km1 depending on the objective function df$Objective
        dplyr::mutate(ObjVal = ifelse(Objective == "cut", Cut, ifelse(Objective == "km1", KM1, NA))) %>%
        aggregate.pre() %>%
        plyr::ddply(by, aggregator) %>%
        aggregate.post() %>%
        dplyr::mutate(AvgCut = ifelse(is.na(AvgCut), Inf, AvgCut)) %>%
        dplyr::mutate(AvgRealCut = ifelse(is.na(AvgRealCut), Inf, AvgRealCut)) %>%
        dplyr::mutate(AvgKM1 = ifelse(is.na(AvgKM1), Inf, AvgKM1)) %>%
        dplyr::mutate(AvgRealKM1 = ifelse(is.na(AvgRealKM1), Inf, AvgRealKM1)) %>%
        dplyr::mutate(AvgObjVal = ifelse(is.na(AvgObjVal), Inf, AvgObjVal)) %>%
        dplyr::mutate(AvgRealObjVal = ifelse(is.na(AvgRealObjVal), Inf, AvgRealObjVal)) %>%
        dplyr::mutate(Infeasible = !Failed & !Timeout & (AvgObjVal == Inf)) %>%
        dplyr::mutate(Imbalanced = !Failed & !Timeout & (AvgObjVal == Inf)) %>%
        dplyr::mutate(Feasible = !Failed & !Timeout & !Infeasible)
}

load_dataset <- \(
    filename, name,
    normalizer = default_normalizer,
    aggregator = default_aggregator,
    default_epsilon = DEFAULT_EPSILON,
    default_timelimit = DEFAULT_TIMELIMIT,
    db = data.frame(),
    eps_offset = 0,
    cache = TRUE) {
    real_filename <- paste0(DATA_INPUT_DIR, "/", filename, ".csv")
    cache_filename <- paste0(CACHE_DIR, "/", gsub("/", "_", filename), ".cached.csv")
    df <- data.frame()

    if (cache && file.exists(cache_filename)) {
        df <- read.csv(cache_filename)
    } else {
        if (!file.exists(real_filename)) {
            cli::cli_alert_danger("CSV file {.path {real_filename}} does not exist")
            quit(status = -1)
        }

        df <- read.csv(real_filename)

        if (!("Cores" %in% colnames(df))) {
            if ("NumNodes" %in% colnames(df) && 
                "NumMPIsPerNode" %in% colnames(df) && 
                "NumThreadsPerMPI" %in% colnames(df)) {
                df$Cores <- df$NumNodes * df$NumMPIsPerNode * df$NumThreadsPerMPI
            } else if ("Threads" %in% colnames(df)) {
                df$Cores <- df$Threads
            } else {
                df$Cores <- 1
            }
        }
        if (!("Failed" %in% colnames(df))) {
            df$Failed <- FALSE
        }
        if (!("Timeout" %in% colnames(df))) {
            df$Timeout <- FALSE
        }
        if (!("Epsilon" %in% colnames(df))) {
            df$Epsilon <- default_epsilon
        }
        df$Epsilon <- df$Epsilon - eps_offset

        df$Algorithm <- name
        df <- normalizer(df, aggregator = aggregator, timelimit = default_timelimit)

        if (cache) {
            cli::cli_alert_info("Caching dataset to {.path {cache_filename}}")
            dir.create(dirname(cache_filename), recursive = TRUE, showWarnings = FALSE)
            write.csv(df, cache_filename, row.names = FALSE)
        }
    }

    if (nrow(db) > 0) {
        db <- db %>% dplyr::mutate(Graph = sub("\\.metis|\\.bgf|\\.mtx|\\.mtx.hgr|\\.hgr|\\.graph|\\.scotch", "", Graph)) 
        df <- df %>% dplyr::left_join(db, by = "Graph")
    }

    df$Algorithm <- name
    df$AvgCut <- ifelse(df$AvgCut == 0, 1, df$AvgCut)
    df$AvgRealCut <- ifelse(df$AvgRealCut == 0, 1, df$AvgRealCut)

    simple <- name
    simple <- stringr::str_replace(simple, "\\\\l", "")
    simple <- stringr::str_replace_all(simple, "\\{[:digit:]*\\}", "")
    simple <- stringr::str_replace_all(simple, "\\{|\\}", "")
    df$SimpleName <- simple

    return(df)
}

load_hypergraph_dataset <- \(
    filename, name,
    normalizer = default_hypergraph_normalizer,
    aggregator = default_hypergraph_aggregator,
    default_epsilon = DEFAULT_EPSILON,
    default_timelimit = DEFAULT_TIMELIMIT,
    db = data.frame(),
    eps_offset = 0,
    cache = TRUE) {
    real_filename <- paste0(DATA_INPUT_DIR, "/", filename, ".csv")
    cache_filename <- paste0(CACHE_DIR, "/", gsub("/", "_", filename), ".cached.csv")
    df <- data.frame()

    if (cache && file.exists(cache_filename)) {
        df <- read.csv(cache_filename)
    } else {
        if (!file.exists(real_filename)) {
            cli::cli_alert_danger("CSV file {.path {real_filename}} does not exist")
            quit(status = -1)
        }

        df <- read.csv(real_filename)

        if (!("Cores" %in% colnames(df))) {
            if ("NumNodes" %in% colnames(df) && 
                "NumMPIsPerNode" %in% colnames(df) && 
                "NumThreadsPerMPI" %in% colnames(df)) {
                df$Cores <- df$NumNodes * df$NumMPIsPerNode * df$NumThreadsPerMPI
            } else if ("Threads" %in% colnames(df)) {
                df$Cores <- df$Threads
            } else {
                df$Cores <- 1
            }
        }
        if (!("Failed" %in% colnames(df))) {
            df$Failed <- FALSE
        }
        if (!("Timeout" %in% colnames(df))) {
            df$Timeout <- FALSE
        }
        if (!("Epsilon" %in% colnames(df))) {
            df$Epsilon <- default_epsilon
        }
        df$Epsilon <- df$Epsilon - eps_offset

        df$Algorithm <- name
        df <- normalizer(df, aggregator = aggregator, timelimit = default_timelimit)

        if (cache) {
            cli::cli_alert_info("Caching dataset to {.path {cache_filename}}")
            dir.create(dirname(cache_filename), recursive = TRUE, showWarnings = FALSE)
            write.csv(df, cache_filename, row.names = FALSE)
        }
    }

    if (nrow(db) > 0) {
        db <- db %>% dplyr::mutate(Hypergraph = sub("\\.zoltan.hg|\\.metis|\\.bgf|\\.mtx|\\.mtx.hgr|\\.hgr|\\.graph|\\.scotch", "", Hypergraph)) 
        df <- df %>% dplyr::left_join(db, by = "Hypergraph")
    }

    df$Algorithm <- name
    df$AvgCut <- ifelse(df$AvgCut == 0, 1, df$AvgCut)
    df$AvgKM1 <- ifelse(df$AvgKM1 == 0, 1, df$AvgKM1)
    df$AvgObjVal <- ifelse(df$AvgObjVal == 0, 1, df$AvgObjVal)
    df$AvgRealCut <- ifelse(df$AvgRealCut == 0, 1, df$AvgRealCut)
    df$AvgRealKM1 <- ifelse(df$AvgRealKM1 == 0, 1, df$AvgRealKM1)
    df$AvgRealObjVal <- ifelse(df$AvgRealObjVal == 0, 1, df$AvgRealObjVal)

    simple <- name
    simple <- stringr::str_replace(simple, "\\\\l", "")
    simple <- stringr::str_replace_all(simple, "\\{[:digit:]*\\}", "")
    simple <- stringr::str_replace_all(simple, "\\{|\\}", "")
    df$SimpleName <- simple

    return(df)
}

default_theme <-
    ggplot2::theme_bw() +
        ggplot2::theme(
            aspect.ratio = 2 / (1 + sqrt(5)) / 1.15,
            legend.background = ggplot2::element_blank(),
            legend.box.spacing = ggplot2::unit(0.1, "cm"),
            legend.text = ggplot2::element_text(size = 10, color = "black"),
            legend.title = ggplot2::element_text(size = 10, color = "black"),
            plot.title = ggplot2::element_text(size = 12, hjust = 0.5, color = "black"),
            strip.background = ggplot2::element_blank(),
            #strip.text = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(linetype = "11", linewidth = 0.3, color = "grey"),
            panel.grid.minor = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(linewidth = 0.2, color = "black"),
            axis.title.y = ggplot2::element_text(size = 10, vjust = 1.5, color = "black"),
            axis.title.x = ggplot2::element_text(size = 10, vjust = 1.5, color = "black"),
            axis.text.x = ggplot2::element_text(size = 10, angle = 0, hjust = 0.5, color = "black"),
            axis.text.y = ggplot2::element_text(size = 10, color = "black")
        )

no_legend_theme <- ggplot2::theme(legend.position = "none")

if (!exists("tikz_device_loaded")) {
    options(tikzDocumentDeclaration = paste0(
        "\\documentclass[a4paper, ngerman, english, cleveref, pdfa, algpseudocode]{", 
        TEX_CLASS, "}"
    ))
    options(tikzLatexPackages = c(
        getOption("tikzLatexPackages"),
        paste0("\\input{", TEX_INPUT, "}")
    ))
    options(tikzMetricsDictionary = ".cache.db")
    tikz_device_loaded <- TRUE
}

current_device_file <- ""
current_device_file_is_tikz <- FALSE

# Wrap math labels only when they will be rendered by LaTeX. Native graphics
# devices otherwise print the dollar delimiters as ordinary characters.
math_labels <- function(labels) {
    if (isTRUE(get0("TEX", ifnotfound = FALSE))) {
        paste0("$", labels, "$")
    } else {
        gsub("[{}]", "", as.character(labels))
    }
}

latex_labels <- function(labels) {
    if (isTRUE(get0("TEX", ifnotfound = FALSE))) labels else gsub("$", "", labels, fixed = TRUE)
}

log_plot_exclusions <- function(data, plot, primary_key, reason = ".exclusion_reason") {
    if (nrow(data) == 0) {
        return(invisible(NULL))
    }

    key_labels <- apply(data[, primary_key, drop = FALSE], 1, function(row) {
        paste(paste0(primary_key, "=", row), collapse = ", ")
    })
    algorithms <- if ("Algorithm" %in% colnames(data)) as.character(data$Algorithm) else "unknown algorithm"
    reasons <- as.character(data[[reason]])
    groups <- split(key_labels, interaction(algorithms, reasons, drop = TRUE, lex.order = TRUE))
    group_rows <- unique(data.frame(Algorithm = algorithms, Reason = reasons, stringsAsFactors = FALSE))

    cli::cli_alert_info("{plot}: excluded {nrow(data)} run{?s} from the plotted comparison set")
    for (i in seq_len(nrow(group_rows))) {
        algorithm <- group_rows$Algorithm[[i]]
        exclusion_reason <- group_rows$Reason[[i]]
        group_name <- interaction(algorithm, exclusion_reason, drop = TRUE, lex.order = TRUE)
        instances <- paste(unique(groups[[as.character(group_name)]]), collapse = "; ")
        cli::cli_bullets(c("*" = "{algorithm} ({exclusion_reason}): {instances}"))
    }

    invisible(NULL)
}

open_pdf <- function(file, width = 7) {
    current_device_file <<- paste0(PDF_OUTPUT, "/", file, ".pdf")
    current_device_file_is_tikz <<- FALSE

    cli::cli_alert_info("Writing PDF file {.path {current_device_file}}")

    pdf(current_device_file, width = width)
}

open_tikz <- function(file, width = 5.5, height = 7, width.factor = 1.0, height.factor = 1.0) {
    current_device_file <<- paste0(TEX_OUTPUT, "/", file, ".tex")
    current_device_file_is_tikz <<- TRUE

    cli::cli_alert_info("Writing TikZ file {.path {current_device_file}}")

    tikzDevice::tikz(
        current_device_file,
        width = width * width.factor,
        height = height * height.factor,
        pointsize = 12,
        timestamp = FALSE
    )
}

open_sink <- function(file) {
    sink(paste0(DATA_OUTPUT, "/", file, "tex"))
}

dev_off <- function() {
    dev.off()
    if (current_device_file_is_tikz) {
        lines <- readLines(con = current_device_file)
        lines <- lines[-which(grepl("\\path\\[clip\\]*", lines, perl = FALSE))]
        lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines, perl = FALSE))]
        writeLines(lines, con = current_device_file)
    }
}

sink_off <- function() {
    sink()
}

make_var_ret <- function(name, value) {
    paste0("\\expandafter\\def\\csname ", name, "\\endcsname{", value, "}\n")
}
make_varf_ret <- function(name, value, digits = 0) {
    make_var_ret(name, sprintf(paste0("%.", digits, "f"), value))
}
make_vari_ret <- function(name, value) {
    make_var_ret(name, round(value))
}
make_var <- function(name, value) {
    cat(make_var_ret(name, value))
}
make_varf <- function(name, value, digits = 0) {
    make_var(name, sprintf(paste0("%.", digits, "f"), value))
}
make_vari <- function(name, value) {
    make_var(name, round(value))
}

Gmean <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
    if (any(x < 0, na.rm = TRUE)) {
        return(NaN)
    }

    if (zero.propagate) {
        if (any(x == 0, na.rm = TRUE)) {
            return(0)
        }
        return(exp(mean(log(x[x != Inf]), na.rm = na.rm)))
    } else {
        x[is.infinite(x)] <- NA
        if (na.rm) {
            x <- na.omit(x)
        }
        stopifnot(!any(is.na(x)))

        return(exp(sum(log(x[x > 0])) / length(x)))
    }
}

Hmean <- function(x) {
    return(length(x) / sum(1.0 / x[x > 0]))
}

filter_df_dist <- function(x, y) dplyr::semi_join(x, y, by = c("Graph", "K", "Nodes"))

filter_df <- function(x, y) dplyr::semi_join(x, y, by = c("Graph", "K"))
common_rows <- \(x, y) dplyr::semi_join(x, y, by = c("Graph", "K"))

common_time_ratios_vararg <- \(
    ..., 
    exclude.timeout = FALSE, 
    exclude.imbalanced = FALSE
) {
    dfs <- list(...)

    row_filter <- \(df) df %>% dplyr::filter(
        (!Failed | Timeout) &
        (!Timeout | !exclude.timeout) &
        (!Imbalanced | !exclude.imbalanced)
    )

    dfs <- lapply(dfs, row_filter)
    subset <- Reduce(common_rows, dfs)

    ans_list <- lapply(dfs, \(df) {
        df_subset <- df %>% common_rows(subset)
        data.frame(
            Algorithm = dplyr::first(df_subset$Algorithm),
            Time = Gmean(df_subset$AvgTime)
        )
    })

    dplyr::bind_rows(ans_list)
}

common_time_ratios <- \(df_a, df_b) {
    joined <- dplyr::inner_join(
        df_a %>% dplyr::filter(Timeout | !Failed),
        df_b %>% dplyr::filter(Timeout | !Failed),
        by = c("Graph", "K"),
        suffix = c(".a", ".b")
    )

    return(joined[["AvgTime.a"]] / joined[["AvgTime.b"]])
}

common_real_cut_ratios <- \(df_a, df_b) {
    joined <- dplyr::inner_join(
        df_a %>% dplyr::filter(!Timeout & !Failed),
        df_b %>% dplyr::filter(!Timeout & !Failed),
        by = c("Graph", "K"),
        suffix = c(".a", ".b")
    )

    return (joined[["AvgRealCut.a"]] / joined[["AvgRealCut.b"]])
}

rename_algo <- \(df, name) df %>% dplyr::mutate(Algorithm = name)

no_axis_title_x <- ggplot2::theme(axis.title.x = ggplot2::element_blank())

ggplot_no_x <- ggplot2::theme(axis.title.x = ggplot2::element_blank())

ggplot_feasibility_scale <- ggplot2::scale_shape_manual(
    values = c(
        "Feasible" = 0, 
        "Imbalanced" = 2, 
        "Timeout" = 3
    ),
    name = "Feasibility"
)
