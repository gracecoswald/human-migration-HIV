# =====================================================================
# region_diversity.R
#
# Region-level HIV-1 variant distributions by time period, and the
# corresponding pie charts (Figure 1A and Supp Figures 1-6; underlying
# data are Supp Table 2).
#
# Run AFTER subtyped_new.R, which supplies HIV_samples: one row per
# record-year, with `region`, `year_category`, the collapsed variant
# columns (A..P, URFs, CRFs, etc.) and `no_years`.
#
# Column names are derived from HIV_samples rather than taken from the
# environment, because subtyped_new.R reassigns CRF_cols to the
# "_adjusted" version partway through.
#
# Outputs:
#   counts_region_time.csv               adjusted counts per region/period
#   proportions_region_time.csv          proportions per region/period

# =====================================================================



# Packages are loaded by 00_libraries.R (see run_all.R).
stopifnot(exists("HIV_samples"))

# Periods shown in the manuscript figures. 1980-1989 and 2020-2022 are
# excluded because they are too sparse to summarise by region.
periods_keep <- c("1990-1994", "1995-1999", "2000-2004",
                  "2005-2009", "2010-2014", "2015-2019")

# ---------------------------------------------------------------------
# 1. Column names, taken from the data
# ---------------------------------------------------------------------
CRF_cols <- grep("^CRF", names(HIV_samples), value = TRUE)
CRF_cols <- c(CRF_cols, "Number of unspecified CRFs")

columns_to_sum <- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L",
                    "N", "O", "P", CRF_cols, "total_CRF", "other_CRF",
                    "URFs", "unspecified_recombinants", "total_recombinants",
                    "Total number genotyped")
columns_to_sum_adjusted <- paste0(columns_to_sum, "_adjusted")

missing_cols <- setdiff(columns_to_sum, names(HIV_samples))
if (length(missing_cols)) {
  stop("Not in HIV_samples: ", paste(missing_cols, collapse = ", "))
}

# Denominator: the variant categories that partition the genotyped
# samples. total_CRF, other_CRF and total_recombinants are roll-ups and
# are excluded here so they are not double-counted.
subtype_cols_adj <- paste0(
  c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P",
    CRF_cols, "URFs", "unspecified_recombinants"), "_adjusted")

# Columns converted to proportions (matches the country-level script)
prop_cols_adj <- paste0(
  c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P",
    CRF_cols, "URFs", "total_recombinants", "total_CRF",
    "unspecified_recombinants"), "_adjusted")

# ---------------------------------------------------------------------
# 2. Adjusted counts by region and time period
# ---------------------------------------------------------------------
reg_sampled <- HIV_samples %>%
  filter(!is.na(region), year_category %in% periods_keep) %>%
  mutate(C = as.numeric(C)) %>%
  mutate(across(all_of(columns_to_sum), ~ . / no_years,
                .names = "{.col}_adjusted")) %>%
  group_by(year_category, region) %>%
  summarise(across(all_of(columns_to_sum_adjusted), ~ sum(., na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(region, year_category)

reg_sampled$total_recorded_genotypes_adj <-
  rowSums(reg_sampled[, subtype_cols_adj], na.rm = TRUE)

write.csv(reg_sampled, "counts_region_time.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 3. Proportions
# ---------------------------------------------------------------------
proportions_regions <- reg_sampled %>%
  mutate(across(all_of(prop_cols_adj), ~ . / total_recorded_genotypes_adj))

write.csv(proportions_regions, "proportions_region_time.csv", row.names = FALSE)

# proportions should sum to 1 across the partition columns
chk <- rowSums(proportions_regions[, subtype_cols_adj], na.rm = TRUE)
if (any(abs(chk - 1) > 1e-6)) {
  warning("Proportions do not sum to 1 in ",
          sum(abs(chk - 1) > 1e-6), " of ", length(chk), " rows")
}
