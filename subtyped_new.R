# =====================================================================
# subtyped_new.R
#
# Takes the REDCap export of the systematic review and produces:
#   counts_country_subtype_distirbutions.csv  adjusted variant counts by
#                                             country and time period
#   proportions.csv                           the same, as proportions
#   RMSDcountries.csv                         pairwise RMSD between every
#                                             country pair in each period
#   indices.csv                               Shannon / Simpson diversity
#                                             and recombinant proportions
#
# Steps
#   1. Load and clean the review data
#   2. Assign regions
#   3. Coerce counts to numeric, split studies across years
#   4. Collapse the variant columns
#   5. Aggregate by country and time period
#   6. Pairwise RMSD
#   7. Diversity indices
# =====================================================================

# Packages are loaded by 00_libraries.R (see run_all.R).

# =====================================================================
# 1. Load and clean
# =====================================================================
HIV_samples <- read_excel("HIVIDDONDPHProject_DATA_LABELS_2024-05-07_1226.xlsx")

HIV_samples <- HIV_samples %>%
  mutate(`Summary of overall risk of study bias` = case_when(
    `Summary score for overall risk of study bias` < 4 ~ "Low Risk",
    `Summary score for overall risk of study bias` > 6 ~ "High Risk",
    `Summary score for overall risk of study bias` > 3 &
      `Summary score for overall risk of study bias` < 7 ~ "Moderate Risk")) %>%
  mutate(`Site 1: Country` = ifelse(
    `Site 1: Country` == "United States Minor Outlying Islands (the)",
    "United States of America (the)", `Site 1: Country`))

# Drop records whose sampling sites span more than one country: only
# single-country records can be assigned to a country distribution.
to_throw <- HIV_samples %>%
  filter(!is.na(.data[[paste0("Site ", 2, ": Country")]])) %>%
  filter(if_any(all_of(paste0("Site ", 2, ": Country")),
                ~ . != .data[[paste0("Site 1: Country")]]))

for (i in 3:30) {
  temp <- HIV_samples %>%
    filter(!is.na(.data[[paste0("Site ", i, ": Country")]])) %>%
    filter(if_any(all_of(paste0("Site ", i, ": Country")),
                  ~ . != .data[[paste0("Site 1: Country")]]))
  to_throw <- plyr::rbind.fill(to_throw, temp)
}

to_throw <- to_throw %>% distinct()

HIV_samples <- HIV_samples %>% anti_join(to_throw)


# =====================================================================
# 2. Regions
#

# =====================================================================
caribbean <- c("Bahamas", "Barbados", "Belize", "Cuba", "Dominican Republic",
               "Dominican Republic (the)", "Guadeloupe", "Haiti", "Jamaica",
               "Martinique", "Puerto Rico", "Trinidad and Tobago")

latin_america <- c("Argentina", "Bolivia", "Bolivia (Plurinational State of)",
                   "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador",
                   "El Salvador", "Guatemala", "Guyana", "French Guiana",
                   "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay",
                   "Peru", "Suriname", "Uruguay", "Venezuela",
                   "Venezuela (Bolivarian Republic of)")

north_america <- c("United States of America", "United States of America (the)",
                   "United States Minor Outlying Islands (the)", "Canada")

# Western and Central Europe
wce <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
         "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany",
         "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Israel",
         "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
         "Netherlands (the)", "Norway", "Poland", "Portugal", "Romania",
         "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
         "Turkey", "United Kingdom of Great Britain & Northern Ireland",
         "United Kingdom of Great Britain and Northern Ireland (the)")

# Eastern Europe & Central Asia
eeca <- c("Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia and Herzegovina",
          "Georgia", "Kazakstan", "Kazakhstan", "Kyrgyzstan", "Montenegro",
          "Republic of Moldova", "Moldova (the Republic of)", "Russian Federation",
          "Russian Federation (the)", "Tajikistan",
          "The former Yugoslav Republic of Macedonia", "Ukraine", "Uzbekistan")

india_nepal_sl <- c("India", "Nepal", "Sri Lanka")

southeast_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "Brunei Darussalam",
                    "Cambodia", "Indonesia", "Lao People's Democratic Republic",
                    "Lao People's Democratic Republic (the)", "Malaysia",
                    "Maldives", "Myanmar", "Pakistan", "Iran (Islamic Republic of)",
                    "Philippines", "Philippines (the)", "Singapore", "Thailand",
                    "Timor-Leste", "Viet Nam")

east_asia <- c("China", "Democratic People's Republic of Korea", "Hong Kong",
               "Japan", "Mongolia", "Republic of Korea", "Korea (the Republic of)",
               "Taiwan (Province of China)")

oceania <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea")

middle_east_north_africa <- c("Algeria", "Egypt", "Kuwait", "Lebanon", "Libya",
                              "Morocco", "Oman", "Saudi Arabia", "Tunisia",
                              "Yemen", "W. Sahara")

west_africa <- c("Benin", "Burkina Faso", "Cameroon", "Cape Verde", "Cabo Verde",
                 "Cote d'Ivoire", "Côte d'Ivoire", "CÃ´te d'Ivoire", "Gambia",
                 "Gambia (the)", "Ghana", "Guinea", "Guinea-Bissau", "Liberia",
                 "Mali", "Mauritania", "Niger", "Niger (the)", "Nigeria",
                 "Senegal", "Sierra Leone", "Togo")

central_africa <- c("Angola", "Central African Republic", "Central African Rep.",
                    "Central African Republic (the)", "Chad", "Congo", "Congo (the)",
                    "Democratic Republic of the Congo",
                    "Congo (the Democratic Republic of the)", "Dem. Rep. Congo",
                    "Equatorial Guinea", "Eq. Guinea", "Gabon")

southern_africa <- c("Botswana", "Eswatini", "eSwatini", "Lesotho", "Malawi",
                     "Mozambique", "Namibia", "South Africa", "Swaziland",
                     "Zambia", "Zimbabwe")

eth_erit_dji <- c("Ethiopia", "Eritrea", "Djibouti")

east_africa <- c("Burundi", "Kenya", "Madagascar", "Mauritius", "Rwanda",
                 "Somalia", "South Sudan", "S. Sudan", "Sudan", "Sudan (the)",
                 "Uganda", "United Republic of Tanzania",
                 "Tanzania, the United Republic of", "Tanzania", "Somaliland")

HIV_samples <- HIV_samples %>%
  mutate(region = case_when(
    `Site 1: Country` %in% caribbean                ~ "caribbean",
    `Site 1: Country` %in% latin_america            ~ "latin_america",
    `Site 1: Country` %in% north_america            ~ "north_america",
    `Site 1: Country` %in% wce                      ~ "wce",
    `Site 1: Country` %in% eeca                     ~ "eeca",
    `Site 1: Country` %in% india_nepal_sl           ~ "india_nepal_sl",
    `Site 1: Country` %in% southeast_asia           ~ "southeast_asia",
    `Site 1: Country` %in% east_asia                ~ "east_asia",
    `Site 1: Country` %in% oceania                  ~ "oceania",
    `Site 1: Country` %in% middle_east_north_africa ~ "middle_east_north_africa",
    `Site 1: Country` %in% west_africa              ~ "west_africa",
    `Site 1: Country` %in% east_africa              ~ "east_africa",
    `Site 1: Country` %in% eth_erit_dji             ~ "eth_erit_dji",
    `Site 1: Country` %in% central_africa           ~ "central_africa",
    `Site 1: Country` %in% southern_africa          ~ "southern_africa",
    TRUE ~ NA_character_
  ))


# =====================================================================
# 3. Column types and study years
#
# Studies spanning several years are split across those years, each year
# receiving 1 / no_years of the study's genotypes.
# =====================================================================

# Genotyping fragment columns, positions 343:352
column_indices <- 343:352
for (i in column_indices) {
  HIV_samples[[i]] <- as.character(HIV_samples[[i]])
}

subtype_cols <- c(grep("^HIV-1 group [MNOP]", names(HIV_samples), value = TRUE),
                  grep("^CRF", names(HIV_samples), value = TRUE),
                  grep("^URF\\d+ number$", names(HIV_samples), value = TRUE),
                  "Number of unspecified CRFs", "Number of undefined URFs",
                  "Unspecified recombinants", "Total number genotyped")

HIV_samples <- HIV_samples %>%
  mutate(across(any_of(subtype_cols), ~ suppressWarnings(as.numeric(.))))

HIV_samples <- HIV_samples %>%
  mutate(`Year study started`     = as.numeric(`Year study started`),
         `Year study ended`       = as.numeric(`Year study ended`),
         `Total number genotyped` = as.numeric(`Total number genotyped`)) %>%
  mutate(`Year study ended` = ifelse(is.na(`Year study ended`),
                                     `Year study started`, `Year study ended`)) %>%
  mutate(no_years           = `Year study ended` - `Year study started` + 1,
         adjusted_genotyped = `Total number genotyped` / no_years,
         freq_var           = 1 / no_years,
         year               = map2(`Year study started`, `Year study ended`, seq)) %>%
  unnest(cols = year)

HIV_samples <- HIV_samples %>%
  mutate(year_category = case_when(
    year >= 1980 & year <= 1989 ~ "1980-1989",
    year >= 1990 & year <= 1994 ~ "1990-1994",
    year >= 1995 & year <= 1999 ~ "1995-1999",
    year >= 2000 & year <= 2004 ~ "2000-2004",
    year >= 2005 & year <= 2009 ~ "2005-2009",
    year >= 2010 & year <= 2014 ~ "2010-2014",
    year >= 2015 & year <= 2019 ~ "2015-2019",
    year >  2019                ~ "2020-2022",
    TRUE ~ NA_character_
  ))


# =====================================================================
# 4. Collapse the variant columns
#
# Sub-subtypes are summed into their parent (A1..A8 into A, F1/F2 into F).
# HIV-2 is not analysed.
# =====================================================================
HIV_samples$A <- rowSums(HIV_samples[, c("HIV-1 group M: A",  "HIV-1 group M: A1",
                                         "HIV-1 group M: A2", "HIV-1 group M: A3",
                                         "HIV-1 group M: A4", "HIV-1 group M: A6",
                                         "HIV-1 group M: A7", "HIV-1 group M: A8")],
                         na.rm = TRUE)
HIV_samples$B   <- HIV_samples$`HIV-1 group M: B`
HIV_samples$C   <- HIV_samples$`HIV-1 group M: C`
HIV_samples$D   <- HIV_samples$`HIV-1 group M: D`
HIV_samples$`F` <- rowSums(HIV_samples[, c("HIV-1 group M: F", "HIV-1 group M: F1",
                                           "HIV-1 group M: F2")], na.rm = TRUE)
HIV_samples$G <- HIV_samples$`HIV-1 group M: G`
HIV_samples$H <- HIV_samples$`HIV-1 group M: H`
HIV_samples$J <- HIV_samples$`HIV-1 group M: J`
HIV_samples$K <- HIV_samples$`HIV-1 group M: K`
HIV_samples$L <- HIV_samples$`HIV-1 group M: L`
HIV_samples$N <- HIV_samples$`HIV-1 group N`
HIV_samples$O <- HIV_samples$`HIV-1 group O`
HIV_samples$P <- HIV_samples$`HIV-1 group P`

# CRFs: all named CRF columns plus the unspecified count
CRF_cols <- grep("^CRF", names(HIV_samples), value = TRUE)
CRF_cols <- c(CRF_cols, "Number of unspecified CRFs")

HIV_samples$total_CRF <- rowSums(HIV_samples[, c(CRF_cols)], na.rm = TRUE)

# CRF01_AE, CRF02_AG and CRF07_BC are shown individually in the figures,
# so other_CRF must exclude all three.
HIV_samples <- HIV_samples %>%
  mutate(CRF01_AE  = replace_na(CRF01_AE, 0),
         CRF02_AG  = replace_na(CRF02_AG, 0),
         other_CRF = total_CRF - (CRF01_AE + CRF02_AG + CRF07_BC))

# URFs: all numbered URF columns plus the undefined count
matching_cols <- grep("^URF\\d+ number$", names(HIV_samples), value = TRUE)
matching_cols <- c(matching_cols, "Number of undefined URFs")

HIV_samples$URFs <- rowSums(HIV_samples[, c(matching_cols)], na.rm = TRUE)

HIV_samples <- HIV_samples %>%
  mutate(unspecified_recombinants = replace_na(`Unspecified recombinants`, 0))

HIV_samples <- HIV_samples %>%
  mutate(total_recombinants = URFs + total_CRF + unspecified_recombinants)


# =====================================================================
# 5. Aggregate by country and time period
# =====================================================================
columns_to_sum <- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L",
                    "N", "O", "P", CRF_cols, "total_CRF", "other_CRF",
                    "URFs", "unspecified_recombinants", "total_recombinants",
                    "Total number genotyped")
columns_to_sum_adjusted <- paste0(columns_to_sum, "_adjusted")

countries_sampled <- HIV_samples %>%
  dplyr::select(
    `Record Number`, no_years, adjusted_genotyped, freq_var, year, year_category,
    `Number of records for this publication (ie sites with seperately reported analyses)`,
    starts_with("Site"), `Total number genotyped`, `Study sites description`,
    `Year study started`, `Year study ended`, `Ethnic Group`, `Country of origin`,
    starts_with("Country of origin"), starts_with("If Other, please specify:"),
    all_of(columns_to_sum)
  ) %>%
  mutate(C = as.numeric(C)) %>%
  mutate_at(vars(columns_to_sum), list(adjusted = ~ . / no_years)) %>%
  group_by(year_category, `Site 1: Country`) %>%
  dplyr::summarise(across(all_of(columns_to_sum_adjusted), ~ sum(., na.rm = TRUE))) %>%
  arrange(`Site 1: Country`, year_category)

# Denominator: the variant categories that partition the genotyped
# samples. total_CRF, other_CRF and total_recombinants are roll-ups and
# are excluded so they are not double-counted.
subtype_totals <- paste0(
  c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P",
    CRF_cols, "URFs", "unspecified_recombinants"), "_adjusted")

countries_sampled$total_recorded_genotypes_adj <-
  rowSums(countries_sampled[, c(subtype_totals)], na.rm = TRUE)

write.csv(countries_sampled, "counts_country_subtype_distirbutions.csv")

# Proportions. The roll-up columns are converted too, for convenience.
subtype_totals <- paste0(
  c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P",
    CRF_cols, "URFs", "total_recombinants", "total_CRF",
    "unspecified_recombinants"), "_adjusted")

proportions <- countries_sampled %>%
  mutate_at(vars(subtype_totals), funs(. / total_recorded_genotypes_adj))

write.csv(proportions, "proportions.csv")


# =====================================================================
# 6. Pairwise RMSD
#
# For each pair of countries in each period, the root mean squared
# deviation between their variant proportion vectors. Lower means more
# similar distributions.
# =====================================================================
calculate_rmsd <- function(df1, df2) {
  if (!all(names(df1) == names(df2))) {
    stop("Dataframes do not have the same structure")
  }
  
  df1_numeric <- df1 %>% ungroup() %>% select(where(is.numeric))
  df2_numeric <- df2 %>% ungroup() %>% select(where(is.numeric))
  
  squared_diff <- (df1_numeric - df2_numeric)^2
  
  sqrt(sum(squared_diff) / ncol(df1_numeric))
}

hiv_data <- proportions %>%
  select(`Site 1: Country`, year_category, all_of(subtype_totals)) %>%
  mutate(across(all_of(subtype_totals), as.numeric))

results <- data.frame(Country_A   = character(),
                      Country_B   = character(),
                      Time_Period = character(),
                      RMSD        = numeric(),
                      stringsAsFactors = FALSE)

time_periods <- unique(hiv_data$year_category)
countries    <- unique(hiv_data$`Site 1: Country`)

for (time_period in time_periods) {
  period_data <- hiv_data %>%
    filter(year_category == time_period) %>%
    select(-year_category)
  
  period_countries <- unique(period_data$`Site 1: Country`)
  
  for (i in 1:(length(period_countries) - 1)) {
    for (j in (i + 1):length(period_countries)) {
      country_a <- period_countries[i]
      country_b <- period_countries[j]
      
      data_a <- period_data %>%
        filter(`Site 1: Country` == country_a) %>%
        select(-`Site 1: Country`)
      
      data_b <- period_data %>%
        filter(`Site 1: Country` == country_b) %>%
        select(-`Site 1: Country`)
      
      rmsd <- calculate_rmsd(data_a, data_b)
      
      results <- rbind(results, data.frame(Country_A   = country_a,
                                           Country_B   = country_b,
                                           Time_Period = time_period,
                                           RMSD        = rmsd,
                                           stringsAsFactors = FALSE))
    }
  }
}

write.csv(results, "RMSDcountries.csv")


# =====================================================================
# 7. Country-level diversity indices
# =====================================================================
subtype_totals <- paste0(
  c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P",
    CRF_cols, "URFs", "unspecified_recombinants"), "_adjusted")

counts_data <- countries_sampled %>%
  ungroup() %>%
  dplyr::select(subtype_totals) %>%
  as.matrix()

simpson_indices <- as.data.frame(diversity(counts_data, index = "simpson"))
shannon_indices <- as.data.frame(diversity(counts_data, index = "shannon"))

indices <- cbind(countries_sampled, simpson_indices)
indices <- cbind(indices, shannon_indices)

indices <- indices %>% mutate(inv_simpson = 1 - simpson_indices)

# Number of distinct variants and of distinct CRFs present
indices$count_subtypes <- rowSums(indices[subtype_totals] > 0)

CRF_cols_adj <- paste0(CRF_cols, "_adjusted")
indices$count_CRFs <- rowSums(indices[CRF_cols_adj] > 0)

indices <- indices %>%
  mutate(proportion_recombinants = (total_recombinants_adjusted / `Total number genotyped_adjusted`) * 100,
         proportion_CRFs         = (total_CRF_adjusted          / `Total number genotyped_adjusted`) * 100,
         proportion_URFs         = (URFs_adjusted               / `Total number genotyped_adjusted`) * 100)

indices <- indices %>%
  dplyr::select(year_category, `Site 1: Country`, simpson_indices, shannon_indices,
                inv_simpson, count_subtypes, count_CRFs, proportion_recombinants,
                proportion_CRFs, proportion_URFs, `Total number genotyped_adjusted`)

write.csv(indices, "indices.csv")