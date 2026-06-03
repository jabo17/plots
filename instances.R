if (TEX) {
    # In TeX mode, you can use LaTeX commands to name your algorithms.
    # For instance, you could decide to use \textsc{} to typeset the names of the algorithms.
    kaminpar_fm_name <- "\\textsc{KaMinPar}-FM"
    kaminpar_name <- "\\textsc{KaMinPar}"
    mtmetis_name <- "\\textsc{MtMetis}"
} else {
    kaminpar_fm_name <- "KaMinPar-FM"
    kaminpar_name <- "KaMinPar"
    mtmetis_name <- "MtMetis"
}

kaminpar_fm <- load_dataset(
    filename = "KaMinPar-FM", 
    name = kaminpar_fm_name
) %>%
    dplyr::filter(K %in% c(8, 37, 64, 91, 128))

mtmetis <- load_dataset(
    filename = "MtMetis", 
    name = mtmetis_name
) %>%
    dplyr::filter(K %in% c(8, 37, 64, 91, 128))

# This step is optional: for the final thesis / paper, it is generally a good idea to 
# fix the colors of the algorithms to ensure consistency across all figures. Thus, we 
# define a "colors" array here, which we can then use in the plotting functions.
set1 <- brewer.pal(n = 9, name = "Set1")

colors <- c()
colors[kaminpar_fm_name] = set1[[1]]
colors[kaminpar_name] = set1[[2]]
colors[mtmetis_name] = set1[[3]]
