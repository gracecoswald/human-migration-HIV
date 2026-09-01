# =====================================================================
# sensitivity_rob.R
#
# Full pipeline for the "excluding high risk of bias" sensitivity analysis.
#
# This mirrors the main analysis exactly - subtyped_new.R,
# migration_RMSD_creation.R, migration_RMSD_adjusted.R and the model
# building - with one filter applied to HIV_samples (PART 1) and all
# outputs suffixed "_rob" so nothing overwrites the main results.
#
# Outputs: RMSDcountries_rob.csv, migration_RMSD_rob.csv,
#          migration_adj_RMSD_rob.csv, migration_adj_RMSD_absdiff_rob.csv,
#          indices_rob.csv, table2_rob.docx, table4_standardised_rob.docx
# =====================================================================


# =====================================================================
# PART 1 - subtype distributions and RMSD (mirrors subtyped_new.R)
# =====================================================================

# obtaining dataset of country distributions and RMSD between countries


# Load the necessary libraries


#HIV_samples<- readxl::read_excel("HIVIDDONDPHProject_DATA_LABELS_2024-05-07_1025.xlsx")


# Packages are loaded by 00_libraries.R (see run_all.R).
HIV_samples<- read_excel("HIVIDDONDPHProject_DATA_LABELS_2024-05-07_1226.xlsx")


HIV_samples<- HIV_samples %>%
  mutate( `Summary of overall risk of study bias` = case_when(`Summary score for overall risk of study bias`<4~ "Low Risk",
                                                              `Summary score for overall risk of study bias`>6 ~ "High Risk",
                                                              (`Summary score for overall risk of study bias`>3 & `Summary score for overall risk of study bias` <7)~ "Moderate Risk")) %>%
  mutate( `Site 1: Country` = ifelse( `Site 1: Country`=="United States Minor Outlying Islands (the)","United States of America (the)", `Site 1: Country` )) 


to_throw<- HIV_samples %>%
  filter(!is.na(.data[[paste0("Site ", 2, ": Country")]])) %>%
  filter(if_any(all_of(paste0("Site ", 2, ": Country")), ~ . != .data[[paste0("Site 1: Country")]]))


for (i in 3:30) {
  temp <- HIV_samples%>%
    filter(!is.na(.data[[paste0("Site ", i, ": Country")]])) %>%
    filter(if_any(all_of(paste0("Site ", i, ": Country")), ~ . != .data[[paste0("Site 1: Country")]]))
  to_throw<- plyr::rbind.fill(to_throw, temp)
}


to_throw<-to_throw%>% distinct()
HIV_samples<- HIV_samples %>% 
  anti_join(to_throw)

# Caribbean
caribbean <- c("Bahamas", "Barbados","Belize", "Cuba", "Dominican Republic", "Dominican Republic (the)", "Guadeloupe", "Haiti", "Jamaica","Martinique", "Puerto Rico", "Trinidad and Tobago")

# Latin America
latin_america <- c("Argentina", "Bolivia", "Bolivia (Plurinational State of)", "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador", "El Salvador", 
                   "Guatemala", "Guyana", "French Guiana", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Suriname", 
                   "Uruguay", "Venezuela", "Venezuela (Bolivarian Republic of)")

north_america<- c("United States of America", "United States of America (the)", "United States Minor Outlying Islands (the)", "Canada" )

# Western and Central Europe and North America (WCENA)
wce <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Czechia", "Denmark", "Estonia", 
         "Finland", "France", "Germany", "Greece","Greenland", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Latvia", 
         "Lithuania", "Luxembourg", "Malta", "Netherlands","Netherlands (the)", "Norway", "Poland", "Portugal", "Romania", "Serbia", 
         "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom of Great Britain & Northern Ireland", "United Kingdom of Great Britain and Northern Ireland (the)")

# Eastern Europe & Central Asia (EECA)
eeca <- c("Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia and Herzegovina", "Georgia", "Kazakstan", "Kazakhstan", "Kyrgyzstan", 
          "Montenegro", "Republic of Moldova", "Moldova (the Republic of)", "Russian Federation", "Russian Federation (the)", "Tajikistan", "The former Yugoslav Republic of Macedonia", 
          "Ukraine", "Uzbekistan")

# South Asia
india_nepal_sl  <- c("India", "Nepal","Sri Lanka")

# South-East Asia
southeast_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic", "Lao People's Democratic Republic (the)",
                    "Malaysia", "Maldives", "Myanmar", "Pakistan", "Iran (Islamic Republic of)" ,"Philippines","Philippines (the)", "Singapore", "Thailand", "Timor-Leste", "Viet Nam")

# East Asia
east_asia <- c("China", "Democratic People's Republic of Korea", "Hong Kong", "Japan", "Mongolia", "Republic of Korea", "Korea (the Republic of)", "Taiwan (Province of China)")

# Oceania
oceania <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea")

# Middle East & North Africa
middle_east_north_africa <- c("Algeria", "Egypt", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman","Saudi Arabia", "Tunisia", "Yemen","W. Sahara")

# West Africa
west_africa <- c("Benin", "Burkina Faso", "Cameroon", "Cape Verde", "Cabo Verde", "Cote d'Ivoire", "Côte d'Ivoire", "CÃ´te d'Ivoire", "Gambia", "Gambia (the)", "Ghana", "Guinea", "Guinea-Bissau", 
                 "Liberia", "Mali", "Mauritania", "Niger", "Niger (the)", "Nigeria", "Senegal", "Sierra Leone", "Togo")


# Central Africa
central_africa <- c("Angola", "Central African Republic","Central African Rep.", "Central African Republic (the)", "Chad", "Congo", "Congo (the)", "Democratic Republic of the Congo", "Congo (the Democratic Republic of the)",   "Dem. Rep. Congo" ,"Equatorial Guinea","Eq. Guinea", "Gabon")

# Southern Africa
southern_africa <- c("Botswana", "Eswatini", "eSwatini", "Lesotho", "Malawi", "Mozambique", "Namibia", "South Africa", "Swaziland", "Zambia", "Zimbabwe")

eth_erit_dji<- c("Ethiopia", "Eritrea", "Djibouti")

east_africa <- c("Burundi", "Kenya", "Madagascar", "Mauritius", "Rwanda", "Somalia", "South Sudan","S. Sudan", "Sudan", "Sudan (the)","Uganda", "United Republic of Tanzania", "Tanzania, the United Republic of", "Tanzania", "Somaliland")

column_indices <- 343:352

# Convert columns to character
for (i in column_indices) {
  HIV_samples[[i]] <- as.character(HIV_samples[[i]])
}

HIV_samples<- HIV_samples %>%
  mutate(region = case_when(
    `Site 1: Country` %in% caribbean ~ "caribbean",
    `Site 1: Country` %in% latin_america ~ "latin_america",
    `Site 1: Country` %in% north_america ~ "north_america",
    `Site 1: Country` %in% wce ~ "wce",
    `Site 1: Country` %in% eeca ~ "eeca",
    `Site 1: Country` %in% india_nepal_sl ~ "india_nepal_sl",
    `Site 1: Country` %in% southeast_asia ~ "southeast_asia",
    `Site 1: Country` %in% east_asia ~ "east_asia",
    `Site 1: Country` %in% oceania ~ "oceania",
    `Site 1: Country` %in% middle_east_north_africa ~ "middle_east_north_africa",
    `Site 1: Country` %in% west_africa ~ "west_africa",
    `Site 1: Country` %in% east_africa ~ "east_africa",
    `Site 1: Country` %in% eth_erit_dji ~ "eth_erit_dji",
    `Site 1: Country` %in% central_africa ~ "central_africa",
    `Site 1: Country` %in% southern_africa ~ "southern_africa",
    TRUE ~ NA_character_
  )) 

subtype_cols <- c(grep("^HIV-1 group [MNOP]", names(HIV_samples), value = TRUE),
                  grep("^CRF", names(HIV_samples), value = TRUE),
                  grep("^URF\\d+ number$", names(HIV_samples), value = TRUE),
                  "Number of unspecified CRFs", "Number of undefined URFs",
                  "Unspecified recombinants", "Total number genotyped")

HIV_samples <- HIV_samples %>%
  mutate(across(any_of(subtype_cols), ~ suppressWarnings(as.numeric(.))))

HIV_samples<-HIV_samples %>%
  mutate(`Year study started` = as.numeric(`Year study started`),
         `Year study ended` = as.numeric(`Year study ended`),
         `Total number genotyped` = as.numeric(`Total number genotyped`))%>%
  mutate(`Year study ended` = ifelse(is.na(`Year study ended`), `Year study started`, `Year study ended`)) %>%
  mutate(no_years = `Year study ended` - `Year study started` +1) %>%
  mutate(adjusted_genotyped = `Total number genotyped` / no_years) %>%
  mutate(freq_var = 1/no_years) %>%
  mutate(year = map2(`Year study started`, `Year study ended`, seq)) %>%
  unnest(cols = year)

HIV_samples<- HIV_samples %>%
  mutate(year_category = case_when(
    year >= 1980 & year <= 1989 ~ "1980-1989",
    year >= 1990 & year <= 1994 ~ "1990-1994",
    year >= 1995 & year <= 1999 ~ "1995-1999",
    year >= 2000 & year <= 2004 ~ "2000-2004",
    year >= 2005 & year <= 2009 ~ "2005-2009",
    year >= 2010 & year <= 2014 ~ "2010-2014",
    year >= 2015 & year <= 2019 ~ "2015-2019",
    year >2019 ~ "2020-2022",
    TRUE ~ NA_character_  # default case, if year does not fall into any category
  )) 


# =====================================================================
# SENSITIVITY ANALYSIS: exclude records at high risk of bias
#
# Records with a missing or "High Risk" overall risk-of-bias assessment
# are dropped. The assessment is derived near the top of this script from
# `Summary score for overall risk of study bias`.
# =====================================================================
message("Before risk-of-bias filter: ", nrow(HIV_samples), " rows")
print(table(HIV_samples$`Summary of overall risk of study bias`, useNA = "ifany"))

HIV_samples <- HIV_samples %>%
  filter(!is.na(`Summary of overall risk of study bias`),
         `Summary of overall risk of study bias` != "High Risk")

message("After risk-of-bias filter:  ", nrow(HIV_samples), " rows")


##obtaining distributions


HIV_samples$A<- rowSums(HIV_samples[, c("HIV-1 group M: A", "HIV-1 group M: A1" ,"HIV-1 group M: A2",	"HIV-1 group M: A3",	"HIV-1 group M: A4",	"HIV-1 group M: A6" ,"HIV-1 group M: A7" ,	"HIV-1 group M: A8")], na.rm=TRUE)
HIV_samples$B<- HIV_samples$`HIV-1 group M: B`
HIV_samples$C<- HIV_samples$`HIV-1 group M: C`
HIV_samples$D<- HIV_samples$`HIV-1 group M: D`
HIV_samples$`F`<- rowSums(HIV_samples[, c("HIV-1 group M: F", "HIV-1 group M: F1" ,"HIV-1 group M: F2")], na.rm=TRUE)
HIV_samples$G <- HIV_samples$`HIV-1 group M: G`
HIV_samples$H <- HIV_samples$`HIV-1 group M: H`
HIV_samples$J <- HIV_samples$`HIV-1 group M: J`
HIV_samples$K <- HIV_samples$`HIV-1 group M: K`
HIV_samples$L <- HIV_samples$`HIV-1 group M: L`

HIV_samples$N<- HIV_samples$`HIV-1 group N`
HIV_samples$O<- HIV_samples$`HIV-1 group O`
HIV_samples$P<- HIV_samples$`HIV-1 group P`

#HIV_samples$`HIV-2:A` <-HIV_samples$`HIV-2 group A`
#HIV_samples$`HIV-2:B` <-HIV_samples$`HIV-2 group B`
#HIV_samples$`HIV-2:C` <-HIV_samples$`HIV-2 group C`
#HIV_samples$`HIV-2:D` <-HIV_samples$`HIV-2 group D`
#HIV_samples$`HIV-2:E` <-HIV_samples$`HIV-2 group E`
#HIV_samples$`HIV-2:F` <-HIV_samples$`HIV-2 group F`
#HIV_samples$`HIV-2:G` <-HIV_samples$`HIV-2 group G`
#HIV_samples$`HIV-2:H` <-HIV_samples$`HIV-2 group H`
#HIV_samples$`HIV-2:01_AB` <-HIV_samples$`HIV-2 group 01_AB`
#HIV_samples$`HIV-2:UNKNOWN` <-HIV_samples$`HIV-2 unknown subtype`


#hiv2_columns <- grep("^HIV-2:", names(HIV_samples), value = TRUE)
#total CRFs


CRF_cols <- grep("^CRF", names(HIV_samples), value = TRUE)
CRF_cols <- c(CRF_cols, "Number of unspecified CRFs")
CRF_cols

HIV_samples$total_CRF <- rowSums(HIV_samples[,c(CRF_cols)], na.rm=TRUE)


# other CRFs 3+
HIV_samples<- HIV_samples %>% 
  mutate(
    CRF01_AE = replace_na(CRF01_AE, 0),
    CRF02_AG = replace_na(CRF02_AG, 0),
    other_CRF = total_CRF - (CRF01_AE + CRF02_AG)
  ) 

#HIV_samples%>% select(`Year study ended`, `Year study started`, total_CRF, CRF_cols)

#total URFs           
matching_cols <- grep("^URF\\d+ number$", names(HIV_samples), value = TRUE)
matching_cols<- c(matching_cols, "Number of undefined URFs")
matching_cols

HIV_samples$URFs <- rowSums(HIV_samples[, c(matching_cols)], na.rm=TRUE)

HIV_samples <- HIV_samples %>%
  mutate(unspecified_recombinants = replace_na(`Unspecified recombinants`, 0))


#total recombinants

HIV_samples<- HIV_samples%>% mutate(total_recombinants = URFs + total_CRF + unspecified_recombinants)


##country-time split
columns_to_sum<- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L", "N", "O", "P", CRF_cols, "total_CRF", "other_CRF", "URFs","unspecified_recombinants", "total_recombinants", "Total number genotyped")
columns_to_sum_adjusted <- paste0(columns_to_sum, "_adjusted")


countries_sampled<- HIV_samples %>%
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
  dplyr::summarise(across(all_of(columns_to_sum_adjusted), ~ sum(. , na.rm = TRUE))) %>%
  arrange(`Site 1: Country`, year_category)


subtype_totals<- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L","N", "O", "P", CRF_cols, "URFs", "unspecified_recombinants")
subtype_totals<- paste0(subtype_totals, "_adjusted")
countries_sampled$total_recorded_genotypes_adj <- rowSums(countries_sampled[, c(subtype_totals)], na.rm=TRUE)


write.csv(countries_sampled, "counts_country_subtype_distirbutions_rob.csv")

#proportions 
subtype_totals<- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L","N", "O", "P", CRF_cols, "URFs","total_recombinants", "total_CRF", "unspecified_recombinants")
subtype_totals<- paste0(subtype_totals, "_adjusted")
proportions<-countries_sampled%>%  mutate_at(vars(subtype_totals), funs(. / total_recorded_genotypes_adj))

write.csv(proportions, "proportions_rob.csv")

calculate_rmsd <- function(df1, df2) {
  if (!all(names(df1) == names(df2))) {
    stop("Dataframes do not have the same structure")
  }
  
  # Ensure both dataframes only contain numeric columns
  df1_numeric <- df1 %>% ungroup() %>% select(where(is.numeric))
  df2_numeric <- df2 %>% ungroup() %>% select(where(is.numeric))
  
  squared_diff <- (df1_numeric - df2_numeric)^2
  
  # Compute the RMSD
  rmsd <- sqrt(sum(squared_diff) / ncol(df1_numeric))
  
  return(rmsd)
}

# Assuming proportions is the dataframe you already have loaded
# Ensure the data is numeric where necessary
hiv_data <- proportions %>% 
  select(`Site 1: Country`, year_category, all_of(subtype_totals)) %>%
  mutate(across(all_of(subtype_totals), as.numeric))


# Initialize an empty dataframe for the results
results <- data.frame(Country_A = character(),
                      Country_B = character(),
                      Time_Period = character(),
                      RMSD = numeric(),
                      stringsAsFactors = FALSE)

# Get unique time periods and countries
time_periods <- unique(hiv_data$year_category)
countries <- unique(hiv_data$`Site 1: Country`)


for (time_period in time_periods) {
  # Filter data for the current time period
  period_data <- hiv_data %>%
    filter(year_category == time_period) %>%
    select(-year_category)
  
  # Get the list of countries in the current time period
  period_countries <- unique(period_data$`Site 1: Country`)
  
  # Iterate over each pair of countries
  for (i in 1:(length(period_countries) - 1)) {
    for (j in (i + 1):length(period_countries)) {
      country_a <- period_countries[i]
      country_b <- period_countries[j]
      
      
      # Filter data for the two countries
      data_a <- period_data %>%
        filter(`Site 1: Country` == country_a) %>%
        select(-`Site 1: Country`)
      
      data_b <- period_data %>%
        filter(`Site 1: Country` == country_b) %>%
        select(-`Site 1: Country`)
      
      
      
      # Calculate the RMSD
      rmsd <- calculate_rmsd(data_a, data_b)
      
      # Add the result to the results dataframe
      results <- rbind(results, data.frame(Country_A = country_a,
                                           Country_B = country_b,
                                           Time_Period = time_period,
                                           RMSD = rmsd,
                                           stringsAsFactors = FALSE))
    }
  }
}

# View the results


print(results)
write.csv(results, "RMSDcountries_rob.csv")

# outputs country level indices
subtype_totals<- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L","N", "O", "P", CRF_cols, "URFs", "unspecified_recombinants")
subtype_totals<- paste0(subtype_totals, "_adjusted")

counts_data <- countries_sampled %>%
  ungroup()%>%
  dplyr::select(subtype_totals)%>%
  as.matrix()

counts_data


# Calculate Simpson and Shannon indices
simpson_indices <- diversity(counts_data, index = "simpson")
shannon_indices <- diversity(counts_data, index = "shannon")

simpson_indices<- as.data.frame(simpson_indices)
shannon_indices<- as.data.frame(shannon_indices)

shannon_indices
countries_sampled

indices<- cbind(countries_sampled, simpson_indices)
indices<- cbind(indices, shannon_indices)

indices <-indices %>% mutate(inv_simpson = 1-simpson_indices)


indices$count_subtypes

indices$count_subtypes <- rowSums(indices[subtype_totals] > 0)

CRF_cols<- paste0(CRF_cols, "_adjusted")
indices$count_CRFs <- rowSums(indices[CRF_cols] > 0)

indices<- indices %>% mutate(proportion_recombinants = (total_recombinants_adjusted/`Total number genotyped_adjusted`)*100,
                             proportion_CRFs = (total_CRF_adjusted/`Total number genotyped_adjusted`)*100,
                             proportion_URFs = (URFs_adjusted/`Total number genotyped_adjusted`)*100)


indices<- indices %>% dplyr::select(year_category, `Site 1: Country`, simpson_indices, shannon_indices, inv_simpson, count_subtypes, count_CRFs, proportion_recombinants, proportion_CRFs, proportion_URFs, `Total number genotyped_adjusted`)

write.csv(indices, "indices_rob.csv")


# =====================================================================
# PART 2 - raw migration joined to RMSD (mirrors migration_RMSD_creation.R)
# =====================================================================


# script to take migration data, join with RMSD countries to obtain full dataset
# including raw bilateral migration flows and RMSD between pairs of countries in
# each time period. Each row is a country-pair-time period


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
# which removes ~9.9 million migrants (mostly Gulf-state corridors).
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
  distinct(country, .keep_all = TRUE)   # first-match-wins, as get_region() did

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

RMSD <- read.csv("RMSDcountries_rob.csv") %>%
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

write.csv(migration_RMSD, "migration_RMSD_rob.csv", row.names = FALSE)


# =====================================================================
# PART 3 - population-weighted migration and prevalence difference (mirrors migration_RMSD_adjusted.R)
# =====================================================================

### 
# Population-weighted bilateral migration joined to RMSD.
# Run AFTER the fixed migration_RMSD script, which supplies:
#   migration_df_final, net_flow, region_lookup, RMSD (with min_country/max_country/year0)


# ---- Population data ---------------------------------------------------
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

write.csv(net_flow_adj, "net_flow_adj_rob.csv", row.names = FALSE)

# ---- Total flow (FIXED) ------------------------------------------------
# Direction is now assigned by matching orig_country, not by row order.
# The old first()/last() version swapped A->B and B->A on ~50% of pairs.
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
# replacing the stale RMSD_normalized and get_region() objects.
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

write.csv(migration_adj_RMSD, "migration_adj_RMSD_rob.csv", row.names = FALSE)


## absolute difference in prevalence

# =======================================================================
# PLHIV prevalence setup + absolute difference in prevalence
# Paste directly beneath the population-weighted script.
# Requires from earlier: migration_df_final, popn_data_avg, RMSD, region_lookup
# =======================================================================


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
  mutate(country = dplyr::recode(country, !!!country_name_corrections))

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

# Direction assigned by matching orig_country, not by row order.
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

write.csv(migration_adj_RMSD_absdiff, "migration_adj_RMSD_absdiff_rob.csv", row.names = FALSE)

# ---- Checks ------------------------------------------------------------
cat("rows:", nrow(migration_adj_RMSD_absdiff),
    " missing abs_diff_prev:", sum(is.na(migration_adj_RMSD_absdiff$abs_diff_prev)), "\n")

# Flows here are the same weighting as migration_adj_RMSD, restricted to
# country-years with PLHIV data, so they should agree wherever both exist.
chk <- migration_adj_RMSD_absdiff %>%
  inner_join(migration_adj_RMSD, by = c("pair_id", "year0"), suffix = c("_ad", "_pop"))
cat("flows agree with migration_adj_RMSD:",
    mean(abs(chk$net_flow_A_to_B_ad - chk$net_flow_A_to_B_pop) < 1e-9), "\n")

migration_adj_RMSD_absdiff %>% arrange(desc(total_flow))


# =====================================================================
# PART 4 - models (mirrors HIVdiversity_modelbuilding_new.Rmd)
# =====================================================================


migration_RMSD<- read.csv("migration_RMSD_rob.csv")
#migration_RMSD_old<- read.csv("migration_RMSD_old.csv")

# population adjusted migration variable 
migration_adj_RMSD<- read.csv("migration_adj_RMSD_rob.csv")

#pop adjusted migration plus absolute difference in prevalence variable
migration_adj_RMSD_absdiff<- read.csv("migration_adj_RMSD_absdiff_rob.csv")

# stadardizing total_flow variable for comparison
migration_RMSD<- migration_RMSD %>% mutate(std_total_flow=scale(total_flow))
migration_adj_RMSD<- migration_adj_RMSD  %>% mutate(std_total_flow=scale(total_flow))
migration_adj_RMSD_absdiff <- migration_adj_RMSD_absdiff%>% mutate(std_total_flow=scale(total_flow))


# time fixed effects only
#total_flow unweighted
model_1 <- fixest::feglm(RMSD ~ total_flow |year0, 
                         data = migration_RMSD,
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))
#    model_1a <- fixest::feglm(RMSD ~ total_flow |year0, 
#                    data = migration_RMSD_old, 
#                    cluster = c("big_region_pairs"),
#                    family = quasipoisson(link = "log"))
# total flow weighted
model_2 <-fixest::feglm(RMSD ~ total_flow |year0, 
                        data = migration_adj_RMSD,
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))


model_3 <-fixest::feglm(RMSD ~ total_flow + abs_diff_prev |year0, 
                        data = migration_adj_RMSD_absdiff,
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))

model_4<- fixest::feglm(RMSD ~ total_flow*abs_diff_prev |year0, 
                        data = migration_adj_RMSD_absdiff,
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))

model_5<- fixest::feglm(RMSD ~ total_flow*as.numeric(year0) + abs_diff_prev |year0, 
                        data = migration_adj_RMSD_absdiff,
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))

model_6 <-fixest::feglm(RMSD ~ total_flow*as.numeric(year0)*abs_diff_prev |year0, 
                        data = migration_adj_RMSD_absdiff, 
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        
                        family = quasipoisson(link = "log")) 

##region and year fixed


model_7 <-fixest::feglm(RMSD ~ total_flow + abs_diff_prev |year0 + big_region_pairs, 
                        data = migration_adj_RMSD_absdiff, 
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))   

model_8<- fixest::feglm(RMSD ~ total_flow*as.numeric(year0) + abs_diff_prev |year0 + big_region_pairs, 
                        data = migration_adj_RMSD_absdiff, 
                        fixef.rm = 'infinite_coef',
                        cluster = "year0",
                        family = quasipoisson(link = "log"))

model_9<-  fixest::feglm(RMSD ~ total_flow*abs_diff_prev |year0 + big_region_pairs, 
                         data = migration_adj_RMSD_absdiff, 
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))


model_10 <-fixest::feglm(RMSD ~ total_flow*as.numeric(year0)*abs_diff_prev |year0 + big_region_pairs, 
                         data = migration_adj_RMSD_absdiff, 
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log")) 


## country and year fixed

model_11 <-fixest::feglm(RMSD ~ total_flow + abs_diff_prev |year0 + pair_id, 
                         data = migration_adj_RMSD_absdiff, 
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))  

model_12<- fixest::feglm(RMSD ~ total_flow*as.numeric(year0) + abs_diff_prev |year0 + pair_id, 
                         data = migration_adj_RMSD_absdiff, 
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))

model_13<-  fixest::feglm(RMSD ~ total_flow*abs_diff_prev |year0 + pair_id, 
                          data = migration_adj_RMSD_absdiff, 
                          fixef.rm = 'infinite_coef',
                          cluster = "year0",
                          family = quasipoisson(link = "log"))


model_14 <- fixest::feglm(RMSD ~ total_flow*as.numeric(year0)*abs_diff_prev |year0 + pair_id, 
                          data = migration_adj_RMSD_absdiff,
                          fixef.rm = 'infinite_coef',
                          cluster = "year0",
                          quasipoisson(link = "log")) 


# standardised regressions

model_15 <-fixest::feglm(RMSD ~ std_total_flow |year0, 
                         data = migration_RMSD, 
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))


model_16 <-fixest::feglm(RMSD ~ std_total_flow |year0, 
                         data = migration_adj_RMSD,
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))


model_17 <-fixest::feglm(RMSD ~ std_total_flow + abs_diff_prev |year0, 
                         data = migration_adj_RMSD_absdiff,
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         
                         family = quasipoisson(link = "log"))


# table 2, main results table
modelsummary(list(model_1, model_2, model_3, model_7, model_10, model_11,   model_14 ), stars = c('*'=.1, '**'=.05  ,'***'=.01), fmt = "%.2e")

# table with standardised migration flow variables (appendix, table 4)
modelsummary(list(model_15, model_16, model_17 ), stars = c('*'=.1, '**'=.05  ,'***'=.01), fmt = "%.2e")

modelsummary(
  list(model_1, model_2, model_3, model_7, model_10, model_11, model_14),
  statistic = "{std.error} (p = {p.value})",
  fmt       = fmt_statistic(estimate  = "%.2e",
                            std.error = "%.2e",
                            p.value   = "%.3f"),
  stars     = c('*' = .1, '**' = .05, '***' = .01),
  output    = "table2_rob.docx"
)

# Appendix Table 4, standardised flow
modelsummary(
  list(model_15, model_16, model_17),
  statistic = "{std.error} (p = {p.value})",
  fmt       = fmt_statistic(estimate  = "%.2e",
                            std.error = "%.2e",
                            p.value   = "%.3f"),
  stars     = c('*' = .1, '**' = .05, '***' = .01),
  output    = "table4_standardised_rob.docx"
)
