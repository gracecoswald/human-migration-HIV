# country level diversity indices



# Packages are loaded by 00_libraries.R (see run_all.R).
outward_migration <- net_flow %>%
  group_by(year0, orig_country) %>%
  summarise(outward_migration = sum(net_flow_pbclosed, na.rm = TRUE)) %>%
  rename( country = orig_country)

# Summarize inward migration
inward_migration <- net_flow %>%
  group_by(year0, dest_country) %>%
  summarise(inward_migration = sum(net_flow_pbclosed, na.rm = TRUE)) %>%
  rename( country = dest_country)

inward_migration


# Merge the two summaries
migration_summary <- full_join(outward_migration, inward_migration, by = c("year0", "country")) %>%
  replace_na(list(outward_migration = 0, inward_migration = 0)) %>%
  mutate( net_migration = inward_migration - outward_migration)


indices<- indices %>% mutate(year0 = as.integer(str_sub(year_category, 1, 4))) %>%
  rename(country = `Site 1: Country`)


migration_summary<- migration_summary %>% left_join(indices, by=c("year0", "country")) %>% filter(!is.na(year_category))
migration_summary

# population data

# population data

popn_data <- read.csv("population_data.csv")

# Clean column names
names(popn_data) <- gsub("^X|..YR.*", "", names(popn_data))

# Reshape the dataframe to long format
popn_data_long <- pivot_longer(popn_data, 
                               cols = matches("^19|^20"), 
                               names_to = "year", 
                               values_to = "population_count")

# Drop unnecessary columns
popn_data_long <- popn_data_long %>% dplyr::select(-Series.Name, -Series.Code)
popn_data_long$year <- as.numeric(popn_data_long$year)

# Convert population_count to numeric
popn_data_long$population_count <- as.numeric(popn_data_long$population_count)

# Filter out years 2021, 2022, 2023
popn_data_long <- popn_data_long %>% filter(year <= 2020)

# Create custom intervals
popn_data_long <- popn_data_long %>%
  mutate(year0 = case_when(
    year >= 1990 & year <= 1994 ~ 1990,
    year >= 1995 & year <= 1999 ~ 1995,
    year >= 2000 & year <= 2004 ~ 2000,
    year >= 2005 & year <= 2009 ~ 2005,
    year >= 2010 & year <= 2014 ~ 2010,
    year >= 2015 & year <= 2019 ~ 2015,
    year == 2020 ~ 2020
  ))

# Group by country and custom interval, then calculate the average population count
popn_data_avg <- popn_data_long %>%
  group_by(Country.Name, year0) %>%
  summarise(avg_popn_count = mean(population_count, na.rm = TRUE), .groups = 'drop')

# Rename columns to desired names
popn_data_avg <- popn_data_avg %>%
  rename(country = Country.Name)

# Filter out any empty country names
popn_data_avg <- popn_data_avg %>% filter(country != "")


popn_data_avg <- popn_data_avg %>%
  dplyr::mutate(country = dplyr::case_when(
    country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    country == "Central African Republic" ~ "Central African Republic (the)",
    country == "Congo, Dem. Rep." ~ "Congo (the Democratic Republic of the)",
    country == "Congo, Rep." ~ "Congo (the)",
    country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    country == "Curacao" ~ "Curaçao",
    country == "Dominican Republic" ~ "Dominican Republic (the)",
    country == "Egypt, Arab Rep." ~ "Egypt",
    country == "Gambia, The" ~ "Gambia (the)",
    country == "Hong Kong SAR, China" ~ "Hong Kong",
    country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    country == "Korea, Dem. People's Rep." ~ "North Korea",
    country == "Korea, Rep." ~ "Korea (the Republic of)",
    country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country == "Lao PDR" ~ "Lao People's Democratic Republic (the)",
    country == "Macao SAR, China" ~ "Macao SAR",
    country == "Micronesia, Fed. Sts." ~ "FS Micronesia",
    country == "Moldova" ~ "Moldova (the Republic of)",
    country == "Netherlands" ~ "Netherlands (the)",
    country == "Niger" ~ "Niger (the)",
    country == "Philippines" ~ "Philippines (the)",
    country == "Russian Federation" ~ "Russian Federation (the)",
    country == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
    country == "Slovak Republic" ~ "Slovakia",
    country == "St. Kitts and Nevis" ~ "St. Kitts & Nevis",
    country == "St. Lucia" ~ "St. Lucia",
    country == "St. Vincent and the Grenadines" ~ "St. Vincent & Grenadines",
    country == "Sudan" ~ "Sudan (the)",
    country == "Tanzania" ~ "Tanzania, the United Republic of",
    country == "Turkiye" ~ "Turkey",
    country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland (the)",
    country == "United States" ~ "United States of America (the)",
    country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    country == "Vietnam" ~ "Viet Nam",
    country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ country  # Keep the original name if no match is found
  ))

# Data for Martinique, guadeloupe and french guiana from worldometer
martinique_data <- data.frame(
  country = rep("Martinique", 7),
  year0 = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
  avg_popn_count = c(374271 ,409942, 432543, 400370, 392181, 383515, 370391)
)


guadeloupe_data <- data.frame(
  country = rep("Guadeloupe", 7),
  year0 = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
  avg_popn_count = c(391951 , 413935, 424067, 403233, 403072, 399089, 395642)
)

guadeloupe_data

french_guiana_data <- data.frame(
  country = rep("French Guiana", 7),
  year0 = c(1990,1995, 2000, 2005, 2010, 2015, 2020),
  avg_popn_count = c(113931 , 137183, 164351, 201259, 228453, 257026, 290969)
)


new_population_data <- bind_rows(martinique_data, guadeloupe_data, french_guiana_data)

# Merge with the existing population data
popn_data_avg <- bind_rows(popn_data_avg, new_population_data)


div_mig_popn<-migration_summary%>% left_join(popn_data_avg, by=c("year0", "country"))%>% filter(!is.na(avg_popn_count))%>% janitor::clean_names()

write.csv(div_mig_popn, "div_mig_popn.csv")

# Step 1: Filter for year0 >= 2000
filtered_data <- div_mig_popn %>% 
  ungroup()%>%
  filter(year0 >= 1995)

all_years <- n_distinct(filtered_data$year0)

countries_with_all_years <- filtered_data %>%
  group_by(country) %>%
  summarise(year_count = n_distinct(year0), .groups = "drop") %>%
  filter(year_count == all_years) %>%
  pull(country)

# Step 3: Filter the original data for those countries
complete_2000 <- filtered_data %>%
  filter(country %in% countries_with_all_years)

# View final dataset

summary_table <- complete_2000 %>% 
  group_by(year0) %>% 
  summarise(  
    country_count = n(),
    sample_count = sum(total_number_genotyped_adjusted),
    
    # Subtypes
    no_subtypes = median(count_subtypes),
    iqr_subtypes = paste0("(", round(quantile(count_subtypes, 0.25), 2), "-", round(quantile(count_subtypes, 0.75), 2), ")"),
    
    # cr_fs
    no_cr_fs = median(count_cr_fs),
    iqr_cr_fs = paste0("(", round(quantile(count_cr_fs, 0.25), 2), "-", round(quantile(count_cr_fs, 0.75), 2), ")"),
    
    # Recombinants
    prop_recombinants = median(proportion_recombinants),
    iqr_recombinants = paste0("(", round(quantile(proportion_recombinants, 0.25), 2), "-", round(quantile(proportion_recombinants, 0.75), 2), ")"),
    
    # cr_fs proportion
    prop_cr_fs = median(proportion_cr_fs),
    iqr_cr_fs_prop = paste0("(", round(quantile(proportion_cr_fs, 0.25), 2), "-", round(quantile(proportion_cr_fs, 0.75), 2), ")"),
    
    # ur_fs proportion
    prop_ur_fs = median(proportion_ur_fs),
    iqr_ur_fs = paste0("(", round(quantile(proportion_ur_fs, 0.25), 2), "-", round(quantile(proportion_ur_fs, 0.75), 2), ")"),
    
    # Shannon Indices
    median_shannon = median(shannon_indices),
    iqr_shannon = paste0("(", round(quantile(shannon_indices, 0.25), 2), "-", round(quantile(shannon_indices, 0.75), 2), ")"),
    mean_shannon = mean(shannon_indices),
    sd_shannon = paste0("(", round(sd(shannon_indices), 2), ")"),
    
    # Simpson Indices
    median_simpson = median(simpson_indices),
    iqr_simpson = paste0("(", round(quantile(simpson_indices, 0.25), 2), "-", round(quantile(simpson_indices, 0.75), 2), ")"),
    mean_simpson = mean(simpson_indices),
    sd_simpson = paste0("(", round(sd(simpson_indices), 2), ")")
  )

# Step 1: Filter the 1990 row and convert it to a numeric vector
baseline_2000 <- summary_table %>% filter(year0 == 2000)
baseline_values <- baseline_2000 %>% select(where(is.numeric)) %>% unlist()

# Step 2: Function to calculate percentage change
calculate_pct_change <- function(current, baseline) {
  round(((current - baseline) / baseline) * 100, 2)
}

# Step 3: Loop through each numeric column and add percentage change columns
for (col in names(baseline_values)) {
  new_col_name <- paste0("pct_change_", col)
  summary_table[[new_col_name]] <- calculate_pct_change(summary_table[[col]], baseline_values[col])
}

# Step 4: Reorder columns to place % change columns after respective IQR/SD
final_col_order <- c()
for (col in colnames(summary_table)) {
  final_col_order <- c(final_col_order, col)
  if (grepl("^iqr|sd", col)) {
    pct_col <- paste0("pct_change_", sub("iqr_|sd_", "", col))
    if (pct_col %in% colnames(summary_table)) {
      final_col_order <- c(final_col_order, pct_col)
    }
  }
}

# Reorder the columns
summary_table <- summary_table %>% select(all_of(final_col_order))

# View the updated table
summary_table


kendall_tau_values <- c()
kendall_p_values <- c()

# List of columns to calculate Kendall's Tau for
columns <- c("no_subtypes", "no_cr_fs", "prop_recombinants", 
             "prop_cr_fs", "prop_ur_fs", "median_shannon", "mean_shannon", "median_simpson", "mean_simpson")

# Calculate Kendall's Tau and p-values for each column
for (col in columns) {
  test_result <- cor.test(summary_table$year0, summary_table[[col]], method = "kendall")
  kendall_tau_values <- c(kendall_tau_values, test_result$estimate)
  kendall_p_values <- c(kendall_p_values, test_result$p.value)
}

# Create rows for Kendall's Tau values and p-values
test_row_tau <- data.frame(
  year0 = "Tau value",  # Placeholder for the year column
  country_count = "",
  sample_count = "",
  no_subtypes = kendall_tau_values[1],
  iqr_subtypes ="",
  no_cr_fs = kendall_tau_values[2],
  iqr_cr_fs="",
  prop_recombinants = kendall_tau_values[3],
  iqr_recombinants="",
  prop_cr_fs = kendall_tau_values[4],
  iqr_cr_fs_prop="",
  prop_ur_fs = kendall_tau_values[5],
  iqr_ur_fs="",
  median_shannon = kendall_tau_values[6],
  iqr_shannon="",
  mean_shannon = kendall_tau_values[7],
  sd_shannon="",
  median_simpson = kendall_tau_values[8],
  iqr_simpson="",
  mean_simpson = kendall_tau_values[9],
  sd_simpson="",
  pct_change_year0 ="",
  pct_change_country_count= "",
  pct_change_sample_count= "",
  pct_change_no_subtypes="",  
  pct_change_no_cr_fs="",
  pct_change_prop_recombinants ="",
  pct_change_prop_cr_fs="",
  pct_change_prop_ur_fs="",
  pct_change_median_shannon="",
  pct_change_mean_shannon="",
  pct_change_median_simpson="",
  pct_change_mean_simpson=""
)

test_row_p <- data.frame(
  year0 = "p-value",  # Placeholder for the year column
  country_count = "",
  sample_count = "",
  no_subtypes = kendall_p_values[1],
  iqr_subtypes ="",
  no_cr_fs = kendall_p_values[2],
  iqr_cr_fs="",
  prop_recombinants = kendall_p_values[3],
  iqr_recombinants="",
  prop_cr_fs = kendall_p_values[4],
  iqr_cr_fs_prop="",
  prop_ur_fs = kendall_p_values[5],
  iqr_ur_fs="",
  median_shannon = kendall_p_values[6],
  iqr_shannon="",
  mean_shannon = kendall_p_values[7],
  sd_shannon="",
  median_simpson = kendall_p_values[8],
  iqr_simpson="",
  mean_simpson = kendall_p_values[9],
  sd_simpson="",
  pct_change_year0 ="",
  pct_change_country_count= "",
  pct_change_sample_count= "",
  pct_change_no_subtypes="",  
  pct_change_no_cr_fs="",
  pct_change_prop_recombinants ="",
  pct_change_prop_cr_fs="",
  pct_change_prop_ur_fs="",
  pct_change_median_shannon="",
  pct_change_mean_shannon="",
  pct_change_median_simpson="",
  pct_change_mean_simpson=""
)

# Bind the new rows to the original table
div_mig_popn_with_tests <- rbind(summary_table, test_row_tau, test_row_p)

div_mig_popn_with_tests <- div_mig_popn_with_tests %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

colnames(div_mig_popn_with_tests)

div_mig_popn_with_tests<- div_mig_popn_with_tests%>% select(c("year0", 
                                                              "country_count", 
                                                              "sample_count",
                                                              
                                                              # Subtypes
                                                              "no_subtypes", "iqr_subtypes", "pct_change_no_subtypes",
                                                              
                                                              # cr_fs
                                                              "no_cr_fs", "iqr_cr_fs", "pct_change_no_cr_fs",
                                                              
                                                              # Recombinants
                                                              "prop_recombinants", "iqr_recombinants", "pct_change_prop_recombinants",
                                                              
                                                              # cr_fs Proportion
                                                              "prop_cr_fs", "iqr_cr_fs_prop", "pct_change_prop_cr_fs",
                                                              
                                                              # ur_fs Proportion
                                                              "prop_ur_fs", "iqr_ur_fs", "pct_change_prop_ur_fs",
                                                              
                                                              # Shannon Indices
                                                              "median_shannon", "iqr_shannon", "pct_change_median_shannon",
                                                              "mean_shannon", "sd_shannon", "pct_change_mean_shannon",
                                                              
                                                              # Simpson Indices
                                                              "median_simpson", "iqr_simpson", "pct_change_median_simpson",
                                                              "mean_simpson", "sd_simpson", "pct_change_mean_simpson"
))

div_mig_popn_with_tests

write.csv(div_mig_popn_with_tests, "complete_1995_diversity.csv")
