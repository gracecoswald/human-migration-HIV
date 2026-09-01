# =====================================================================
# 00_libraries.R
#
# Every package used anywhere in the analysis, loaded in one place.
# Sourced by run_all.R before any analysis script; the individual
# scripts contain no library() calls of their own.
#
#   source("00_libraries.R")
# =====================================================================


required_packages <- c(
  # data manipulation
  "dplyr", "tidyr", "tibble", "stringr", "purrr", "readxl", "readr",
  
  # modelling
  "fixest",         # fixed-effects quasi-Poisson (all models)
  "alpaca",         # feglm.nb, negative binomial comparison only
  "modelsummary",   # Tables 2 and 4
  "flextable",      # Word output for modelsummary and Table 3
  "corrplot",       # collinearity plot in model_building.R
  
  # diversity indices
  "vegan",
  
  # plotting
  "ggplot2", "RColorBrewer", "scales", "gridExtra", "grid", "patchwork",
  
  # maps and chord diagrams
  "sf", "rnaturalearth", "rnaturalearthdata", "png", "circlize"
)

missing <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------------------------------------------------
# Namespace conflicts
#
# car, plyr and MASS all mask dplyr verbs. None is attached here, but if
# one is loaded later, these calls in the scripts are namespaced so they
# keep working: dplyr::select, dplyr::filter, dplyr::recode.
# ---------------------------------------------------------------------
message("Packages loaded: ", length(required_packages))
