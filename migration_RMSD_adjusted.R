### 
# Population-weighted bilateral migration joined to RMSD.
# Run AFTER the fixed migration_RMSD script, which supplies:
#   migration_df_final, net_flow, region_lookup, RMSD (with min_country/max_country/year0)


# ---- Population data ---------------------------------------------------

# Packages are loaded by 00_libraries.R (see run_all.R).
popn_data <- read.csv("population_data.csv")
names(popn_data) <- gsub("^X|..YR.*", "", names(popn_data))

popn_data_long <- popn_data %>%
  pivot_longer(cols = matches("^19|^20"),
               names_to = "year", values_to = "population_count") %>%
  dplyr::select(-Series.Name, -Series.Code) %>%
  mutate(year = as.numeric(year),
         population_count = as.numeric(population_count)) %>%
  filter(year <= 2020) %>%
  mutate(year0 = case_when(
    year == 1990 ~ 1990,
    year == 1995 ~ 1995,
    year >= 2000 & year <= 2004 ~ 2000,
    year >= 2005 & year <= 2009 ~ 2005,
    year >= 2010 & year <= 2014 ~ 2010,
    year >= 2015 & year <= 2019 ~ 2015,
    year == 2020 ~ 2020
  ))

popn_data_avg <- popn_data_long %>%
  group_by(Country.Name, year0) %>%
  summarise(avg_popn_count = mean(population_count, na.rm = TRUE), .groups = "drop") %>%
  rename(country = Country.Name) %>%
  filter(country != "")

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
    TRUE ~ country
  ))

# Martinique / Guadeloupe / French Guiana (Worldometer)
new_population_data <- bind_rows(
  data.frame(country = "Martinique", year0 = c(1990,1995,2000,2005,2010,2015,2020),
             avg_popn_count = c(374271,409942,432543,400370,392181,383515,370391)),
  data.frame(country = "Guadeloupe", year0 = c(1990,1995,2000,2005,2010,2015,2020),
             avg_popn_count = c(391951,413935,424067,403233,403072,399089,395642)),
  data.frame(country = "French Guiana", year0 = c(1990,1995,2000,2005,2010,2015,2020),
             avg_popn_count = c(113931,137183,164351,201259,228453,257026,290969))
)

popn_data_avg <- bind_rows(popn_data_avg, new_population_data)

# ---- Check for destinations with no population match -------------------
missing_pop <- migration_df_final %>%
  distinct(dest_country, year0) %>%
  anti_join(popn_data_avg, by = c("dest_country" = "country", "year0" = "year0"))
if (nrow(missing_pop) > 0) {
  warning("No population match for ", nrow(missing_pop),
          " destination-year combinations; their adjusted flows will be NA.")
  print(missing_pop)
}

# ---- Weight by destination population ----------------------------------
migration_df_weighted <- migration_df_final %>%
  left_join(popn_data_avg, by = c("year0" = "year0", "dest_country" = "country")) %>%
  mutate(
    dest_population   = avg_popn_count,
    da_min_open_adj   = (da_min_open   / dest_population) * 100000,
    da_min_closed_adj = (da_min_closed / dest_population) * 100000,
    da_pb_closed_adj  = (da_pb_closed  / dest_population) * 100000
  )

net_flow_adj <- migration_df_weighted %>%
  group_by(orig, dest, year0) %>%
  mutate(
    net_flow_min_open_adj   = sum(da_min_open_adj),
    net_flow_min_closed_adj = sum(da_min_closed_adj),
    net_flow_pbclosed_adj   = sum(da_pb_closed_adj)
  ) %>%
  dplyr::select(
    year0, orig, orig_country, dest, dest_country,
    region_origin, region_dest, plot_area_origin, plot_area_dest,
    net_flow_pbclosed_adj
  ) %>%
  distinct() %>%
  ungroup()

write.csv(net_flow_adj, "net_flow_adj.csv", row.names = FALSE)

# ---- Total flow  ------------------------------------------------

total_flow_adj <- net_flow_adj %>%
  ungroup() %>%
  mutate(
    country_A = pmin(orig_country, dest_country),   # alphabetically first
    country_B = pmax(orig_country, dest_country),   # alphabetically second
    pair_id   = paste(country_A, country_B, sep = "-")
  ) %>%
  group_by(pair_id, country_A, country_B, year0) %>%
  summarise(
    net_flow_A_to_B = sum(net_flow_pbclosed_adj[orig_country == country_A], na.rm = TRUE),
    net_flow_B_to_A = sum(net_flow_pbclosed_adj[orig_country == country_B], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_flow = net_flow_A_to_B + net_flow_B_to_A)

# ---- Join to RMSD ------------------------------------------------------
# Uses RMSD (already carrying min_country/max_country) and region_lookup,

migration_adj_RMSD <- RMSD %>%
  inner_join(
    total_flow_adj %>% rename(min_country = country_A, max_country = country_B),
    by = c("min_country", "max_country", "year0")
  ) %>%
  filter(!is.na(RMSD)) %>%
  left_join(region_lookup, by = c("min_country" = "country")) %>%
  rename(big_region_A = region) %>%
  left_join(region_lookup, by = c("max_country" = "country")) %>%
  rename(big_region_B = region) %>%
  mutate(
    big_region_pairs = paste(pmin(big_region_A, big_region_B),
                             pmax(big_region_A, big_region_B), sep = "-")
  ) %>%
  arrange(min_country, max_country, year0) %>%
  dplyr::select(
    country_A = min_country, country_B = max_country,
    Time_Period, year0, RMSD,
    net_flow_A_to_B, net_flow_B_to_A, total_flow,
    pair_id, big_region_A, big_region_B, big_region_pairs
  )

write.csv(migration_adj_RMSD, "migration_adj_RMSD.csv", row.names = FALSE)


## absolute difference in prevalence



# ---- Population attached to BOTH origin and destination ----------------
pop_migration_df_final <- migration_df_final %>%
  left_join(popn_data_avg, by = c("year0" = "year0", "dest_country" = "country")) %>%
  mutate(dest_population = avg_popn_count) %>%
  dplyr::select(-avg_popn_count) %>%
  left_join(popn_data_avg, by = c("year0" = "year0", "orig_country" = "country")) %>%
  mutate(orig_population = avg_popn_count) %>%
  dplyr::select(-avg_popn_count)

# ---- PLHIV data --------------------------------------------------------
plhiv_data <- read_xlsx("PLHIV_data.xlsx")

country_name_corrections <- c(
  "Afghanistan" = "Afghanistan",
  "Australia" = "Australia",
  "Bangladesh" = "Bangladesh",
  "Bhutan" = "Bhutan",
  "Brunei Darussalam" = "Brunei Darussalam",
  "Cambodia" = "Cambodia",
  "China" = "China",
  "Democratic People Republic of Korea" = "Korea (the Democratic People's Republic of)",
  "Fiji" = "Fiji",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Iran (Islamic Republic of)" = "Iran (Islamic Republic of)",
  "Japan" = "Japan",
  "Lao People Democratic Republic" = "Lao People's Democratic Republic (the)",
  "Malaysia" = "Malaysia",
  "Maldives" = "Maldives",
  "Mongolia" = "Mongolia",
  "Myanmar" = "Myanmar",
  "Nepal" = "Nepal",
  "New Zealand" = "New Zealand",
  "Pakistan" = "Pakistan",
  "Papua New Guinea" = "Papua New Guinea",
  "Philippines" = "Philippines (the)",
  "Republic of Korea" = "Korea (the Republic of)",
  "Singapore" = "Singapore",
  "Sri Lanka" = "Sri Lanka",
  "Thailand" = "Thailand",
  "Timor-Leste" = "Timor-Leste",
  "Viet Nam" = "Viet Nam",
  "Caribbean" = "Caribbean",
  "Bahamas" = "Bahamas (the)",
  "Barbados" = "Barbados",
  "Belize" = "Belize",
  "Cuba" = "Cuba",
  "Dominican Republic" = "Dominican Republic (the)",
  "Guyana" = "Guyana",
  "Haiti" = "Haiti",
  "Jamaica" = "Jamaica",
  "Suriname" = "Suriname",
  "Trinidad and Tobago" = "Trinidad and Tobago",
  "Angola" = "Angola",
  "Botswana" = "Botswana",
  "Comoros" = "Comoros (the)",
  "Eritrea" = "Eritrea",
  "Eswatini" = "Eswatini",
  "Ethiopia" = "Ethiopia",
  "Kenya" = "Kenya",
  "Lesotho" = "Lesotho",
  "Madagascar" = "Madagascar",
  "Malawi" = "Malawi",
  "Mauritius" = "Mauritius",
  "Mozambique" = "Mozambique",
  "Namibia" = "Namibia",
  "Rwanda" = "Rwanda",
  "South Africa" = "South Africa",
  "South Sudan" = "South Sudan",
  "Uganda" = "Uganda",
  "United Republic of Tanzania" = "Tanzania, the United Republic of",
  "Zambia" = "Zambia",
  "Zimbabwe" = "Zimbabwe",
  "Albania" = "Albania",
  "Armenia" = "Armenia",
  "Azerbaijan" = "Azerbaijan",
  "Belarus" = "Belarus",
  "Bosnia and Herzegovina" = "Bosnia and Herzegovina",
  "Georgia" = "Georgia",
  "Kazakhstan" = "Kazakhstan",
  "Kyrgyzstan" = "Kyrgyzstan",
  "Montenegro" = "Montenegro",
  "North Macedonia" = "North Macedonia",
  "Republic of Moldova" = "Moldova (the Republic of)",
  "Russian Federation" = "Russian Federation (the)",
  "Tajikistan" = "Tajikistan",
  "Turkmenistan" = "Turkmenistan",
  "Ukraine" = "Ukraine",
  "Uzbekistan" = "Uzbekistan",
  "Argentina" = "Argentina",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Brazil" = "Brazil",
  "Chile" = "Chile",
  "Colombia" = "Colombia",
  "Costa Rica" = "Costa Rica",
  "Ecuador" = "Ecuador",
  "El Salvador" = "El Salvador",
  "Guatemala" = "Guatemala",
  "Honduras" = "Honduras",
  "Mexico" = "Mexico",
  "Nicaragua" = "Nicaragua",
  "Panama" = "Panama",
  "Paraguay" = "Paraguay",
  "Peru" = "Peru",
  "Uruguay" = "Uruguay",
  "Venezuela" = "Venezuela (Bolivarian Republic of)",
  "Algeria" = "Algeria",
  "Bahrain" = "Bahrain",
  "Djibouti" = "Djibouti",
  "Egypt" = "Egypt",
  "Jordan" = "Jordan",
  "Kuwait" = "Kuwait",
  "Lebanon" = "Lebanon",
  "Libya" = "Libya",
  "Morocco" = "Morocco",
  "Oman" = "Oman",
  "Qatar" = "Qatar",
  "Saudi Arabia" = "Saudi Arabia",
  "Somalia" = "Somalia",
  "Sudan" = "Sudan (the)",
  "Syrian Arab Republic" = "Syrian Arab Republic",
  "Tunisia" = "Tunisia",
  "United Arab Emirates" = "United Arab Emirates",
  "Yemen" = "Yemen",
  "Iraq" = "Iraq",
  "Benin" = "Benin",
  "Burkina Faso" = "Burkina Faso",
  "Burundi" = "Burundi",
  "Cameroon" = "Cameroon",
  "Cape Verde" = "Cabo Verde",
  "Central African Republic" = "Central African Republic (the)",
  "Chad" = "Chad",
  "Congo" = "Congo (the)",
  "Cote dIvoire" = "Côte d'Ivoire",
  "Democratic Republic of the Congo" = "Congo (the Democratic Republic of the)",
  "Equatorial Guinea" = "Equatorial Guinea",
  "Gabon" = "Gabon",
  "Gambia" = "Gambia (the)",
  "Ghana" = "Ghana",
  "Guinea" = "Guinea",
  "Guinea-Bissau" = "Guinea-Bissau",
  "Liberia" = "Liberia",
  "Mali" = "Mali",
  "Mauritania" = "Mauritania",
  "Niger" = "Niger (the)",
  "Nigeria" = "Nigeria",
  "Senegal" = "Senegal",
  "Sierra Leone" = "Sierra Leone",
  "Togo" = "Togo",
  "Sao Tome and Principe" = "Sao Tome and Principe",
  "Austria" = "Austria",
  "Belgium" = "Belgium",
  "Bulgaria" = "Bulgaria",
  "Canada" = "Canada",
  "Croatia" = "Croatia",
  "Cyprus" = "Cyprus",
  "Czech Republic" = "Czechia",
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Finland" = "Finland",
  "France" = "France",
  "Germany" = "Germany",
  "Greece" = "Greece",
  "Hungary" = "Hungary",
  "Iceland" = "Iceland",
  "Ireland" = "Ireland",
  "Israel" = "Israel",
  "Italy" = "Italy",
  "Latvia" = "Latvia",
  "Lithuania" = "Lithuania",
  "Luxembourg" = "Luxembourg",
  "Malta" = "Malta",
  "Netherlands" = "Netherlands (the)",
  "Norway" = "Norway",
  "Poland" = "Poland",
  "Portugal" = "Portugal",
  "Romania" = "Romania",
  "Serbia" = "Serbia",
  "Slovakia" = "Slovakia",
  "Slovenia" = "Slovenia",
  "Spain" = "Spain",
  "Sweden" = "Sweden",
  "Switzerland" = "Switzerland",
  "Türkiye" = "Turkey",
  "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland (the)",
  "United States of America" = "United States of America (the)"
)

plhiv <- plhiv_data %>%
  rename("country" = `...4`) %>%
  mutate(country = recode(country, !!!country_name_corrections))

plhiv_avg <- plhiv %>%
  pivot_longer(cols = matches("^19|^20"),
               names_to = "year", values_to = "plhiv_count") %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2020) %>%
  mutate(year0 = case_when(
    year >= 1990 & year <= 1994 ~ 1990,
    year >= 1995 & year <= 1999 ~ 1995,
    year >= 2000 & year <= 2004 ~ 2000,
    year >= 2005 & year <= 2009 ~ 2005,
    year >= 2010 & year <= 2014 ~ 2010,
    year >= 2015 & year <= 2019 ~ 2015,
    year == 2020 ~ 2020
  )) %>%
  group_by(country, year0) %>%
  summarise(avg_plhiv = mean(plhiv_count, na.rm = TRUE), .groups = "drop")

# Countries in the RMSD pairs with no PLHIV match (expect French Guiana,
# Greenland, Guadeloupe, Hong Kong, Martinique, Puerto Rico)
setdiff(migration_RMSD$country_B, plhiv$country)

# ---- Prevalence per country-year --------------------------------------
# One row per country-year, so prevalence can be attached to either side of
# a pair without depending on migration direction.
prev_lookup <- plhiv_avg %>%
  inner_join(popn_data_avg, by = c("country", "year0")) %>%
  mutate(prev = (avg_plhiv / avg_popn_count) * 100) %>%
  dplyr::select(country, year0,
                plhiv = avg_plhiv, population = avg_popn_count, prev)

plhiv_pop_migration_df_final <- pop_migration_df_final %>%
  left_join(plhiv_avg, by = c("year0" = "year0", "dest_country" = "country")) %>%
  mutate(dest_plhiv = avg_plhiv) %>%
  dplyr::select(-avg_plhiv) %>%
  left_join(plhiv_avg, by = c("year0" = "year0", "orig_country" = "country")) %>%
  mutate(orig_plhiv = avg_plhiv) %>%
  dplyr::select(-avg_plhiv) %>%
  filter(!is.na(dest_plhiv), !is.na(orig_plhiv)) %>%
  mutate(orig_prev = (orig_plhiv / orig_population) * 100,
         dest_prev = (dest_plhiv / dest_population) * 100)

# ---- Absolute difference in prevalence ---------------------------------
migration_df_absdiff <- plhiv_pop_migration_df_final %>%
  mutate(
    da_min_open_adj   = (da_min_open   / dest_population) * 100000,
    da_min_closed_adj = (da_min_closed / dest_population) * 100000,
    da_pb_closed_adj  = (da_pb_closed  / dest_population) * 100000
  )

net_flow_absdiff <- migration_df_absdiff %>%
  group_by(orig, dest, year0) %>%
  mutate(
    net_flow_min_open_adj   = sum(da_min_open_adj),
    net_flow_min_closed_adj = sum(da_min_closed_adj),
    net_flow_pbclosed_adj   = sum(da_pb_closed_adj)
  ) %>%
  dplyr::select(
    year0, orig, orig_country, dest, dest_country,
    region_origin, region_dest, plot_area_origin, plot_area_dest,
    net_flow_pbclosed_adj
  ) %>%
  distinct() %>%
  ungroup()

# Direction assigned by matching orig_country
total_flow_absdiff <- net_flow_absdiff %>%
  ungroup() %>%
  mutate(
    country_A = pmin(orig_country, dest_country),
    country_B = pmax(orig_country, dest_country),
    pair_id   = paste(country_A, country_B, sep = "-")
  ) %>%
  group_by(pair_id, country_A, country_B, year0) %>%
  summarise(
    net_flow_A_to_B = sum(net_flow_pbclosed_adj[orig_country == country_A], na.rm = TRUE),
    net_flow_B_to_A = sum(net_flow_pbclosed_adj[orig_country == country_B], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_flow = net_flow_A_to_B + net_flow_B_to_A)

migration_adj_RMSD_absdiff <- RMSD %>%
  inner_join(
    total_flow_absdiff %>% rename(min_country = country_A, max_country = country_B),
    by = c("min_country", "max_country", "year0")
  ) %>%
  filter(!is.na(RMSD)) %>%
  left_join(region_lookup, by = c("min_country" = "country")) %>%
  rename(big_region_A = region) %>%
  left_join(region_lookup, by = c("max_country" = "country")) %>%
  rename(big_region_B = region) %>%
  left_join(prev_lookup, by = c("min_country" = "country", "year0" = "year0")) %>%
  rename(plhiv_A = plhiv, population_A = population, prev_A = prev) %>%
  left_join(prev_lookup, by = c("max_country" = "country", "year0" = "year0")) %>%
  rename(plhiv_B = plhiv, population_B = population, prev_B = prev) %>%
  mutate(
    big_region_pairs = paste(pmin(big_region_A, big_region_B),
                             pmax(big_region_A, big_region_B), sep = "-"),
    abs_diff_prev = abs(prev_A - prev_B)
  ) %>%
  arrange(min_country, max_country, year0) %>%
  dplyr::select(
    country_A = min_country, country_B = max_country,
    Time_Period, year0, RMSD,
    net_flow_A_to_B, net_flow_B_to_A, total_flow,
    pair_id, big_region_A, big_region_B, big_region_pairs,
    plhiv_A, population_A, prev_A,
    plhiv_B, population_B, prev_B,
    abs_diff_prev
  )

write.csv(migration_adj_RMSD_absdiff, "migration_adj_RMSD_absdiff.csv", row.names = FALSE)

# ---- Checks ------------------------------------------------------------
cat("rows:", nrow(migration_adj_RMSD_absdiff),
    " missing abs_diff_prev:", sum(is.na(migration_adj_RMSD_absdiff$abs_diff_prev)), "\n")


chk <- migration_adj_RMSD_absdiff %>%
  inner_join(migration_adj_RMSD, by = c("pair_id", "year0"), suffix = c("_ad", "_pop"))
cat("flows agree with migration_adj_RMSD:",
    mean(abs(chk$net_flow_A_to_B_ad - chk$net_flow_A_to_B_pop) < 1e-9), "\n")

migration_adj_RMSD_absdiff %>% arrange(desc(total_flow))
