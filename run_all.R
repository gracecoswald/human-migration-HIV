# =====================================================================
# run_all.R
#
# Runs the whole analysis in order. Each script is sourced into the
# global environment, so objects created by one are available to the
# next. See README.md for what each script does.
#
# Inputs expected in the working directory:
#   HIVIDDONDPHProject_DATA_LABELS_2024-05-07_1226.xlsx
#   migration_df_final.csv
#   population_data.csv
#   PLHIV_data.xlsx
#
# Run time is dominated by subtyped_new.R (pairwise RMSD over all
# country pairs and time periods) and by the pie chart generation.
# =====================================================================

source("00_libraries.R")

run <- function(script) {
  message("\n========== ", script, " ==========")
  t0 <- Sys.time()
  source(script, echo = FALSE)
  message("done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}

# ---- Core analysis --------------------------------------------------
run("subtyped_new.R")             # variant distributions, RMSD, diversity indices
run("migration_RMSD_creation.R")  # raw migration joined to RMSD
run("migration_RMSD_adjusted.R")  # population-weighted flows + prevalence difference
run("model_building.R")           # Table 2, Appendix Table 4, Figure 4

# ---- Descriptive tables ---------------------------------------------
run("country_diversity.R")        # country diversity indices over time
run("region_distributions.R")     # regional variant distributions over time
run("region_migration.R")         # Table 3, region-to-region flow matrix

# ---- Figures --------------------------------------------------------
run("pie_chart_generation.R")     # country and region pie charts, legends
run("global_chord_diagrams.R")    # Figure 1B, global chord diagrams
run("africa_figures.R")           # Figure 3, Africa maps and chord diagrams
run("region_maps.R")              # Figure 2 and equivalents, regional maps

# ---- Sensitivity analyses -------------------------------------------
# Each is a self-contained rerun of the whole pipeline with one filter
# applied, writing outputs suffixed _pol / _rob. Run in a clean session
# if possible: they redefine objects used above.
# run("sensitivity_pol.R")        # pol / full-length fragments only
# run("sensitivity_rob.R")        # excluding high risk of bias

message("\nAll scripts complete.")
