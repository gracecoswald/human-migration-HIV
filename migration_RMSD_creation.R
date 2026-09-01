# script to take migration data, join with RMSD countries to obtain full dataset
# including raw bilateral migration flows and RMSD between pairs of countries in
# each time period. Each row is a country-pair-time period



# Packages are loaded by 00_libraries.R (see run_all.R).

migration_df_final <- read.csv("migration_df_final.csv")

net_flow <- migration_df_final %>%
  group_by(orig, dest, year0) %>%
  mutate(
    net_flow_min_open   = sum(da_min_open),
    net_flow_min_closed = sum(da_min_closed),
    net_flow_pbclosed   = sum(da_pb_closed)
  ) %>%
  dplyr::select(
    year0, orig, orig_country, dest, dest_country,
    region_origin, region_dest, plot_area_origin, plot_area_dest,
    net_flow_pbclosed
  ) %>%
  distinct() %>%
  ungroup()

total_flow <- net_flow %>%
  ungroup() %>%
  mutate(
    country_A = pmin(orig_country, dest_country),   # alphabetically first
    country_B = pmax(orig_country, dest_country),   # alphabetically second
    pair_id   = paste(country_A, country_B, sep = "-")
  ) %>%
  group_by(pair_id, country_A, country_B, year0) %>%
  summarise(
    net_flow_A_to_B = sum(net_flow_pbclosed[orig_country == country_A], na.rm = TRUE),
    net_flow_B_to_A = sum(net_flow_pbclosed[orig_country == country_B], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_flow = net_flow_A_to_B + net_flow_B_to_A)

# =====================================================================
# Region definitions
#
# Defined in full here rather than inherited from subtyped_new.R, which
# declares vectors of the same names covering only countries present in
# the HIV subtype data. The migration data covers every country, so the
# extension block below is required - without it 64 countries have no
# region and are silently dropped by the filter in final_region_flow,
# which removes ~9.9 million migrants 
# =====================================================================

# ---- Base lists (as in subtyped_new.R) ------------------------------
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

wce <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
         "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany",
         "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Israel",
         "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
         "Netherlands (the)", "Norway", "Poland", "Portugal", "Romania",
         "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
         "Turkey", "United Kingdom of Great Britain & Northern Ireland",
         "United Kingdom of Great Britain and Northern Ireland (the)")

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

# ---- Extension: countries present in the migration data only --------
# NOTE: Maldives appears in both southeast_asia (above) and india_nepal_sl
# (below). region_lists order decides, and india_nepal_sl comes first, so
# Maldives is classified there. No Maldives records exist in the RMSD data,
# so this affects the region flow matrices only.
caribbean <- c(caribbean,
               "Anguilla", "Antigua & Barbuda", "Aruba", "Bermuda",
               "British Virgin Islands", "Curaçao", "Dominica", "Grenada",
               "Montserrat", "Sint Maarten", "St. Helena", "St. Kitts & Nevis",
               "St. Lucia", "St. Pierre & Miquelon", "St. Vincent & Grenadines",
               "Turks & Caicos Islands", "U.S. Virgin Islands", "Cayman Islands")

latin_america <- c(latin_america, "Falkland Islands", "French Guiana")

wce <- c(wce, "Andorra", "Faroe Islands", "Gibraltar", "Isle of Man",
         "Liechtenstein", "Monaco", "San Marino")

eeca <- c(eeca, "North Macedonia", "Serbia & Montenegro", "Turkmenistan")

india_nepal_sl <- c(india_nepal_sl, "Maldives")

southeast_asia <- c(southeast_asia, "Brunei", "Timor-Leste", "Myanmar")

east_asia <- c(east_asia, "North Korea", "Macao SAR")

oceania <- c(oceania,
             "American Samoa", "Cook Islands", "Fiji", "FS Micronesia", "Guam",
             "Kiribati", "Marshall Islands", "Nauru", "New Caledonia", "Niue",
             "Northern Mariana Islands", "Palau", "Samoa", "Solomon Islands",
             "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis & Futuna",
             "French Polynesia")

middle_east_north_africa <- c(middle_east_north_africa,
                              "Bahrain", "Iraq", "Jordan", "Palestine", "Qatar",
                              "United Arab Emirates", "Syria", "Western Sahara")

west_africa <- c(west_africa, "São Tomé & Príncipe")

central_africa <- c(central_africa, "Comoros", "Mayotte", "Réunion", "Seychelles")

east_africa <- c(east_africa, "Mauritius")

# ---- Lookup ---------------------------------------------------------
region_lists <- list(
  latin_america            = latin_america,
  caribbean                = caribbean,
  north_america            = north_america,
  wce                      = wce,
  eeca                     = eeca,
  india_nepal_sl           = india_nepal_sl,
  southeast_asia           = southeast_asia,
  east_asia                = east_asia,
  oceania                  = oceania,
  middle_east_north_africa = middle_east_north_africa,
  west_africa              = west_africa,
  east_africa              = east_africa,
  eth_erit_dji             = eth_erit_dji,
  central_africa           = central_africa,
  southern_africa          = southern_africa
)

region_lookup <- region_lists %>%
  tibble::enframe(name = "region", value = "country") %>%
  tidyr::unnest(country) %>%
  distinct(country, .keep_all = TRUE)   

# Every country in the migration data must have a region, or its flows are
# silently dropped below.
unassigned <- setdiff(
  unique(c(migration_df_final$orig_country, migration_df_final$dest_country)),
  region_lookup$country)
if (length(unassigned)) {
  stop("No region for ", length(unassigned), " countries: ",
       paste(unassigned, collapse = ", "))
}

# =====================================================================
final_region_flow <- net_flow %>%
  ungroup() %>%
  left_join(region_lookup, by = c("orig_country" = "country")) %>%
  rename(orig_reg = region) %>%
  left_join(region_lookup, by = c("dest_country" = "country")) %>%
  rename(dest_reg = region) %>%
  filter(!is.na(orig_reg), !is.na(dest_reg)) %>%
  group_by(year0, orig_reg, dest_reg) %>%
  summarise(total_flow = sum(net_flow_pbclosed, na.rm = TRUE), .groups = "drop")

region_grouped_flow <- final_region_flow %>%
  group_by(orig_reg, dest_reg) %>%
  summarise(total_flow = sum(total_flow, na.rm = TRUE), .groups = "drop")

RMSD <- read.csv("RMSDcountries.csv") %>%
  mutate(
    year0       = as.integer(str_sub(Time_Period, 1, 4)),
    min_country = pmin(Country_A, Country_B),
    max_country = pmax(Country_A, Country_B)
  )

migration_RMSD <- RMSD %>%
  inner_join(
    total_flow %>% rename(min_country = country_A, max_country = country_B),
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

write.csv(migration_RMSD, "migration_RMSD.csv", row.names = FALSE)
