# HIV-1 variant diversity and international migration

Analysis code for *"Human migration and the global distribution of HIV-1 genetic variants"*


---

## Quick start

```r
source("run_all.R")
```

Everything runs in order and writes its outputs to the working directory.

### Required input files

| File | Contents |
|---|---|
| `HIVIDDONDPHProject_DATA_LABELS_2024-05-07_1226.xlsx` | REDCap export of the global molecular epidemiology database, one row per study record |
| `migration_df_final.csv` | Bilateral migration flow estimates by country pair and time period |
| `population_data.csv` | World Bank country populations by year |
| `PLHIV_data.xlsx` | UNAIDS people living with HIV, by country and year |

### Software

R 4.5 or later. `00_libraries.R` installs anything missing.

---

## Scripts

Run in this order. `run_all.R` does it for you.

### Setup

| Script | Purpose |
|---|---|
| `00_libraries.R` | Loads every package used anywhere. No analysis script contains `library()` calls. |
| `ne_country_names.R` | Maps our country names to `rnaturalearth`'s (`"Congo (the Democratic Republic of the)"` to `"Dem. Rep. Congo"`). Sourced by the mapping scripts. |

### Core analysis

| Script | Reads | Writes | Purpose |
|---|---|---|---|
| `subtyped_new.R` | REDCap xlsx | `counts_country_subtype_distirbutions.csv`, `proportions.csv`, `RMSDcountries_test_.csv`, `indices.csv` | Cleans the review data, assigns regions, collapses variant columns, splits multi-year studies across years, aggregates by country and time period, computes pairwise RMSD between every country pair, and computes Shannon and Simpson diversity indices. |
| `migration_RMSD_creation.R` | `migration_df_final.csv`, RMSD | `migration_RMSD_test.csv` | Sums bidirectional migration per country pair, assigns regions to both sides, joins to RMSD. Also builds `net_flow` and `final_region_flow`, used by the figure scripts. |
| `migration_RMSD_adjusted.R` | migration, population, PLHIV | `migration_adj_RMSD_test.csv`, `migration_adj_RMSD_absdiff_test.csv` | Weights flows by destination population (per 100,000), and computes the absolute difference in HIV prevalence between each pair. |
| `model_building.R` | the three joined datasets | `table2_main_results.docx`, `table4_standardised.docx`, `predicted_RMSD_prev.png` | Fixed-effects quasi-Poisson models of RMSD on migration flow, prevalence difference and time. Produces Table 2, Appendix Table 4 and Figure 4. |

### Descriptive tables

| Script | Writes | Purpose |
|---|---|---|
| `country_diversity.R` | `complete_1995_diversity.csv` | Diversity indices over time for countries with data in every period, with tests. |
| `region_distributions.R` | `counts_region_time.csv`, `proportions_region_time.csv` | Regional variant distributions by time period (Supp Table 2). |
| `region_migration.R` | `region_flow_matrix_*.csv`, `total_region_migration_new.csv` | Region-to-region migration matrices (supp Table 3) and immigrant / emigrant / within-region totals. |

### Figures

| Script | Writes | Purpose |
|---|---|---|
| `pie_chart_generation.R` | `pie_charts_country/`, `pie_charts_region/`, legend PDFs | One variant pie chart per country and per region per period, plus the colour legends. Feeds the map scripts. |
| `global_chord_diagrams.R` | `chord_diagrams/` | Global chord diagrams of regional migration flows (Figure 1B). |
| `africa_figures.R` | `africa_maps/`, `chord_africa/` | Africa maps with flows and pie charts, and Africa chord diagrams (Figure 3). |
| `region_maps.R` | `regional_maps/` | Maps for the Americas, Europe and Africa, Europe and North America, Europe, and Asia (Figure 2 and equivalents). |

### Sensitivity analyses

Each is a self-contained rerun of the entire pipeline with one filter applied,
writing outputs suffixed `_pol` or `_rob`. They mirror the main scripts exactly
apart from that filter. **Run in a clean R session** — they redefine objects
used by the main analysis.

| Script | Filter |
|---|---|
| `sensitivity_pol.R` | Only records where every genotyping fragment is *pol* or full length |
| `sensitivity_rob.R` | Excludes records assessed as high risk of bias |

---




## Output naming

Main analysis outputs carry the `_test` suffix from development; sensitivity
outputs carry `_pol` and `_rob`. Nothing overwrites anything else.
