# =====================================================================
# africa_figures.R
#
# Africa maps and Africa chord diagrams, one per time period (Figure 3
# and the equivalents for earlier periods).
#
#   A. Map of Africa: regions shaded, bilateral migration flows drawn as
#      curved arrows, country HIV-1 variant pie charts overlaid.
#   B. Chord diagram of migration flows between the six African regions.
#
# Run AFTER:
#   migration_RMSD_creation.R  - supplies net_flow and final_region_flow
#   pie_chart_generation.R     - supplies country_pie_files
#
# Outputs:
#   africa_maps/Africa_<period>.png
#   chord_africa/chord_africa_<period>.png
# =====================================================================



# Packages are loaded by 00_libraries.R (see run_all.R).
stopifnot(exists("net_flow"), exists("final_region_flow"),
          exists("country_pie_files"))

if (!dir.exists("africa_maps"))  dir.create("africa_maps")
if (!dir.exists("chord_africa")) dir.create("chord_africa")

periods <- c("1990" = "1990-1994", "1995" = "1995-1999", "2000" = "2000-2004",
             "2005" = "2005-2009", "2010" = "2010-2014", "2015" = "2015-2019")

# ---------------------------------------------------------------------
# Country names as used by rnaturalearth
# ---------------------------------------------------------------------
country_name_corrections <- c(
  #latin america and caribbean   
  "Anguilla" = "Anguilla",
  "Antigua & Barbuda" = "Antigua and Barb.",
  "Aruba" = "Aruba",
  "Bahamas" = "Bahamas",
  "Barbados" = "Barbados",
  "British Virgin Islands" = "British Virgin Is.",
  "Cayman Islands" = "Cayman Is.",
  "Cuba" = "Cuba",
  "Dominica" = "Dominica",
  "Dominican Republic (the)" = "Dominican Rep.",
  "Grenada" = "Grenada",
  "Guadeloupe" = "Guadeloupe",
  "Haiti" = "Haiti",
  "Jamaica" = "Jamaica",
  "Martinique" = "Martinique",
  "Montserrat" = "Montserrat",
  "Puerto Rico" = "Puerto Rico",
  "St. Kitts & Nevis" = "St. Kitts and Nevis",
  "St. Lucia" = "Saint Lucia",
  "St. Vincent & Grenadines" = "St. Vin. and Gren.",
  "Trinidad and Tobago" = "Trinidad and Tobago",
  "Turks & Caicos Islands" = "Turks and Caicos Is.",
  "U.S. Virgin Islands" = "U.S. Virgin Is.",
  "Belize" = "Belize",
  "Costa Rica" = "Costa Rica",
  "El Salvador" = "El Salvador",
  "Guatemala" = "Guatemala",
  "Honduras" = "Honduras",
  "Mexico" = "Mexico",
  "Nicaragua" = "Nicaragua",
  "Panama" = "Panama",
  "Argentina" = "Argentina",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Brazil" = "Brazil",
  "Chile" = "Chile",
  "Colombia" = "Colombia",
  "Ecuador" = "Ecuador",
  "Falkland Islands" = "Falkland Is.",
  "French Guiana" = "French Guiana",
  "Guyana" = "Guyana",
  "Paraguay" = "Paraguay",
  "Peru" = "Peru",
  "Suriname" = "Suriname",
  "Uruguay" = "Uruguay",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Sint Maarten" = "Sint Maarten",
  "Curaçao" = "Curaçao",
  
  #Africa
  "Burundi" = "Burundi",
  "Comoros" = "Comoros",
  "Djibouti" = "Djibouti",
  "Eritrea" = "Eritrea",
  "Ethiopia" = "Ethiopia",
  "Kenya" = "Kenya",
  "Madagascar" = "Madagascar",
  "Malawi" = "Malawi",
  "Mauritius" = "Mauritius",
  "Mayotte" = "Mayotte",
  "Mozambique" = "Mozambique",
  "Réunion" = "Réunion",
  "Rwanda" = "Rwanda",
  "Seychelles" = "Seychelles",
  "Somalia" = "Somalia",
  "Uganda" = "Uganda",
  "Tanzania, the United Republic of" = "Tanzania",
  "Zambia" = "Zambia",
  "Zimbabwe" = "Zimbabwe",
  "Angola" = "Angola",
  "Cameroon" = "Cameroon",
  "Central African Republic (the)" = "Central African Rep.",
  "Chad" = "Chad",
  "Congo (the)" = "Congo",
  "Congo (the Democratic Republic of the)" = "Dem. Rep. Congo",
  "Equatorial Guinea" = "Eq. Guinea",
  "Gabon" = "Gabon",
  "São Tomé & Príncipe" = "São Tomé and Principe",
  "Algeria" = "Algeria",
  "Egypt" = "Egypt",
  "Libya" = "Libya",
  "Morocco" = "Morocco",
  "Sudan (the)" = "Sudan",
  "Tunisia" = "Tunisia",
  "Western Sahara" = "W. Sahara",
  "Botswana" = "Botswana",
  "Eswatini" = "eSwatini",
  "Lesotho" = "Lesotho",
  "Namibia" = "Namibia",
  "South Africa" = "South Africa",
  "Benin" = "Benin",
  "Burkina Faso" = "Burkina Faso",
  "Cabo Verde" = "Cabo Verde",
  "Côte d'Ivoire" = "Côte d'Ivoire",
  "Gambia (the)" = "Gambia",
  "Ghana" = "Ghana",
  "Guinea" = "Guinea",
  "Guinea-Bissau" = "Guinea-Bissau",
  "Liberia" = "Liberia",
  "Mali" = "Mali",
  "Mauritania" = "Mauritania",
  "Niger (the)" = "Niger",
  "Nigeria" = "Nigeria",
  "St. Helena" = "Saint Helena",
  "Senegal" = "Senegal",
  "Sierra Leone" = "Sierra Leone",
  "Togo" = "Togo",
  "South Sudan" = "S. Sudan",
  "Seychelles" = "Seychelles",
  
  #Europe
  "Uzbekistan" = "Uzbekistan",
  "Kyrgyzstan" = "Kyrgyzstan",
  "Tajikistan" = "Tajikistan",
  "Turkmenistan" = "Turkmenistan",
  "Kazakhstan" = "Kazakhstan",
  "Belarus" = "Belarus",
  "Bulgaria" = "Bulgaria",
  "Czechia" = "Czechia",
  "Hungary" = "Hungary",
  "Poland" = "Poland",
  "Moldova (the Republic of)" = "Moldova",
  "Romania" = "Romania",
  "Russian Federation (the)" = "Russia",
  "Slovakia" = "Slovakia",
  "Ukraine" = "Ukraine",
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Faroe Islands" = "Faeroe Is.",
  "France" = "France",
  "Finland" = "Finland",
  "Iceland" = "Iceland",
  "Ireland" = "Ireland",
  "Isle of Man" = "Isle of Man",
  "Latvia" = "Latvia",
  "Lithuania" = "Lithuania",
  "Norway" = "Norway",
  "Sweden" = "Sweden",
  "United Kingdom of Great Britain and Northern Ireland (the)" = "United Kingdom",
  "Albania" = "Albania",
  "Andorra" = "Andorra",
  "Bosnia and Herzegovina" = "Bosnia and Herz.",
  "Croatia" = "Croatia",
  "Gibraltar" = "Gibraltar",
  "Greece" = "Greece",
  "Italy" = "Italy",
  "Malta" = "Malta",
  "North Macedonia" = "North Macedonia",
  "Portugal" = "Portugal",
  "San Marino" = "San Marino",
  "Serbia & Montenegro" = "Serbia",
  "Slovenia" = "Slovenia",
  "Spain" = "Spain",
  "Austria" = "Austria",
  "Belgium" = "Belgium",
  "France" = "France",
  "Liechtenstein" = "Liechtenstein",
  "Luxembourg" = "Luxembourg",
  "Monaco" = "Monaco",
  "Netherlands (the)" = "Netherlands",
  "Switzerland" = "Switzerland",
  "Montenegro" = "Montenegro",
  "Vatican" = "Vatican",
  "Jersey" = "Jersey",
  "Guernsey" = "Guernsey",
  "Åland" = "Åland",
  
  #Asia Oceania
  "China" = "China",
  "Hong Kong" = "Hong Kong",
  "Macao SAR" = "Macao",
  "North Korea" = "North Korea",
  "Japan" = "Japan",
  "Mongolia" = "Mongolia",
  "Korea (the Republic of)" = "South Korea",
  "Brunei" = "Brunei",
  "Cambodia" = "Cambodia",
  "Indonesia" = "Indonesia",
  "Lao People's Democratic Republic (the)" = "Laos",
  "Malaysia" = "Malaysia",
  "Myanmar" = "Myanmar",
  "Philippines (the)" = "Philippines",
  "Singapore" = "Singapore",
  "Thailand" = "Thailand",
  "Timor-Leste" = "Timor-Leste",
  "Viet Nam" = "Vietnam",
  "Afghanistan" = "Afghanistan",
  "Bangladesh" = "Bangladesh",
  "Bhutan" = "Bhutan",
  "India" = "India",
  "Iran (Islamic Republic of)" = "Iran",
  "Maldives" = "Maldives",
  "Nepal" = "Nepal",
  "Pakistan" = "Pakistan",
  "Sri Lanka" = "Sri Lanka",
  "Armenia" = "Armenia",
  "Azerbaijan" = "Azerbaijan",
  "Bahrain" = "Bahrain",
  "Cyprus" = "Cyprus",
  "Georgia" = "Georgia",
  "Iraq" = "Iraq",
  "Israel" = "Israel",
  "Jordan" = "Jordan",
  "Kuwait" = "Kuwait",
  "Lebanon" = "Lebanon",
  "Oman" = "Oman",
  "Qatar" = "Qatar",
  "Saudi Arabia" = "Saudi Arabia",
  "Palestine" = "Palestine",
  "Syria" = "Syria",
  "Turkey" = "Turkey",
  "United Arab Emirates" = "United Arab Emirates",
  "Yemen" = "Yemen",
  "Australia" = "Australia",
  "New Zealand" = "New Zealand",
  "Fiji" = "Fiji",
  "New Caledonia" = "New Caledonia",
  "Papua New Guinea" = "Papua New Guinea",
  "Solomon Islands" = "Solomon Is.",
  "Vanuatu" = "Vanuatu",
  "Guam" = "Guam",
  "Kiribati" = "Kiribati",
  "Marshall Islands" = "Marshall Is.",
  "FS Micronesia" = "Micronesia",
  "Nauru" = "Nauru",
  "Northern Mariana Islands" = "N. Mariana Is.",
  "Palau" = "Palau",
  "American Samoa" = "American Samoa",
  "Cook Islands" = "Cook Is.",
  "French Polynesia" = "Fr. Polynesia",
  "Niue" = "Niue",
  "Samoa" = "Samoa",
  "Tokelau" = "Tokelau",
  "Tonga" = "Tonga",
  "Tuvalu" = "Tuvalu",
  "Wallis & Futuna" = "Wallis and Futuna Is.",
  
  #North America
  
  "United States of America (the)"= "United States of America"
)

# ---------------------------------------------------------------------
# Base map, region shading and country centroids
# ---------------------------------------------------------------------
world  <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world %>% filter(region_un == "Africa")

# Region vectors are the HIV-side lists; map them to natural earth names
# so they match the polygon names.
ne_name <- function(x) ifelse(x %in% names(country_name_corrections),
                              country_name_corrections[x], x)

region_assignments <- bind_rows(
  data.frame(name = ne_name(middle_east_north_africa), region = "Middle East & North Africa"),
  data.frame(name = ne_name(west_africa),              region = "West Africa"),
  data.frame(name = ne_name(central_africa),           region = "Central Africa"),
  data.frame(name = ne_name(southern_africa),          region = "Southern Africa"),
  data.frame(name = ne_name(eth_erit_dji),             region = "Horn of Africa"),
  data.frame(name = ne_name(east_africa),              region = "East Africa"),
  .id = NULL
) %>% distinct(name, .keep_all = TRUE)

africa <- africa %>% left_join(region_assignments, by = "name")

unshaded <- africa %>% filter(is.na(region)) %>% pull(name)
if (length(unshaded)) {
  message("No region shading for: ", paste(unshaded, collapse = ", "))
}

region_colors <- c(
  "Middle East & North Africa" = alpha("#4B9CD3", 0.2),
  "West Africa"                = alpha("gray",    0.2),
  "East Africa"                = alpha("#B2CC24", 0.3),
  "Horn of Africa"             = alpha("#FECC2F", 0.3),
  "Southern Africa"            = alpha("#F66320", 0.3),
  "Central Africa"             = alpha("#FBA127", 0.3)
)

coords <- suppressWarnings(africa %>% st_centroid() %>% st_coordinates())
country_coords <- data.frame(name = africa$name,
                             lon  = coords[, 1],
                             lat  = coords[, 2])

# Pie chart index, with country names mapped to natural earth names
pie_index <- country_pie_files %>%
  rename(country = name) %>%
  mutate(country = ne_name(country)) %>%
  left_join(country_coords, by = c("country" = "name")) %>%
  filter(!is.na(lon), !is.na(lat))

# ---------------------------------------------------------------------
# Arrow scaling (as in the original)
# ---------------------------------------------------------------------
arrow_length <- function(flow) log10(flow) * 0.15

size_breaks  <- c(5000, 10000, 50000, 100000, 150000, 200000, 300000,
                  500000, 1000000, 1500000, 2000000, 2500000, 3000000)
size_labels  <- comma(size_breaks)
alpha_breaks <- log10(size_breaks) * 0.1
alpha_labels <- comma(size_breaks)

flow_min <- 5000   # only flows at or above this are drawn

# ---------------------------------------------------------------------
# A. One map per period
# ---------------------------------------------------------------------
for (yr in names(periods)) {
  period <- periods[[yr]]
  
  migration_data <- net_flow %>%
    filter(plot_area_origin %in% c("Sub-Saharan Africa", "North Africa"),
           plot_area_dest   %in% c("Sub-Saharan Africa", "North Africa"),
           orig_country != "Seychelles", dest_country != "Seychelles",
           year0 == as.integer(yr),
           net_flow_pbclosed >= flow_min) %>%
    mutate(orig_country = ne_name(orig_country),
           dest_country = ne_name(dest_country)) %>%
    left_join(country_coords, by = c("orig_country" = "name")) %>%
    rename(origin_lon = lon, origin_lat = lat) %>%
    left_join(country_coords, by = c("dest_country" = "name")) %>%
    rename(dest_lon = lon, dest_lat = lat) %>%
    filter(!is.na(origin_lon), !is.na(origin_lat),
           !is.na(dest_lon),   !is.na(dest_lat)) %>%
    filter(!(origin_lon == dest_lon & origin_lat == dest_lat)) %>%
    arrange(desc(net_flow_pbclosed)) %>%
    mutate(arrow_len = arrow_length(net_flow_pbclosed),
           alpha     = log10(net_flow_pbclosed) * 0.1)
  
  base_map <- ggplot() +
    geom_sf(data = africa, aes(fill = region), color = "black", size = 0.2) +
    scale_fill_manual(values = region_colors, name = "Region") +
    geom_curve(data = migration_data,
               aes(x = origin_lon, y = origin_lat,
                   xend = dest_lon, yend = dest_lat,
                   size = net_flow_pbclosed, alpha = alpha),
               arrow = arrow(length = unit(migration_data$arrow_len, "cm"),
                             type = "open"),
               color = "blue", curvature = 0.2, lineend = "round") +
    scale_size_continuous(name = "Flow Size", breaks = size_breaks,
                          labels = size_labels, range = c(0.1, 1.5)) +
    scale_alpha_continuous(name = "Flow Size", breaks = alpha_breaks,
                           labels = alpha_labels, range = c(0.2, 1)) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.text      = element_text(size = 14),
      legend.title     = element_text(size = 16),
      legend.key.size  = unit(2, "lines"),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank()
    ) +
    coord_sf(xlim = c(-25, 60), ylim = c(-35, 38), expand = FALSE) +
    guides(fill = "none")
  
  # Overlay the country pie charts for this period
  pies <- pie_index %>% filter(year_category == period)
  
  final_map <- base_map
  for (i in seq_len(nrow(pies))) {
    final_map <- final_map +
      annotation_custom(
        rasterGrob(readPNG(pies$file[i]), interpolate = TRUE),
        xmin = pies$lon[i] - 1.5, xmax = pies$lon[i] + 1.5,
        ymin = pies$lat[i] - 1.5, ymax = pies$lat[i] + 1.5
      )
  }
  
  file <- file.path("africa_maps", paste0("Africa_", period, ".png"))
  ggsave(file, plot = final_map, width = 20, height = 15, units = "in", dpi = 300)
  message("wrote ", file, "  (", nrow(migration_data), " flows, ",
          nrow(pies), " pies)")
}

# ---------------------------------------------------------------------
# B. Africa chord diagrams, one per period
# ---------------------------------------------------------------------
africa_regions <- c("middle_east_north_africa", "west_africa", "east_africa",
                    "eth_erit_dji", "central_africa", "southern_africa")

d1 <- data.frame(
  region = africa_regions,
  order1 = seq_along(africa_regions),
  col1   = c("#4B9CD3", "grey", "#B2CC24", "#FECC2F", "#FBA127", "#F66320"),
  reg1   = c("Middle East", "West", "East", "Ethiopia", "Central", "Southern"),
  reg2   = c("& North Africa", "Africa", "Africa", ", Eritrea & Djibouti",
             "Africa", "Africa"),
  stringsAsFactors = FALSE
)

d0 <- final_region_flow %>%
  filter(orig_reg %in% africa_regions, dest_reg %in% africa_regions) %>%
  mutate(total_flow = total_flow / 1e06)   # millions of individuals

draw_africa_chord <- function(dat, file, label) {
  png(file = file, height = 7, width = 7, units = "in", res = 500)
  
  circos.clear()
  par(mar = rep(0, 4), cex = 1)
  circos.par(start.degree = 90, track.margin = c(-0.1, 0.1),
             gap.degree = 4, points.overflow.warning = FALSE)
  
  chordDiagram(
    x = dat %>% dplyr::select(orig_reg, dest_reg, total_flow),
    directional = 1,
    order = d1$region,
    grid.col = setNames(d1$col1, d1$region),
    annotationTrack = "grid",
    transparency = 0.25,
    annotationTrackHeight = c(0.05, 0.1),
    direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow",
    diffHeight = -0.04,
    link.sort = TRUE,
    link.largest.ontop = TRUE
  )
  
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% filter(region == sector.index) %>% pull(reg2)
    
    circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 3, 4),
                labels = reg1, facing = "bending", cex = 0.7)
    circos.text(x = mean(xlim), y = 2.75, labels = reg2,
                facing = "bending", cex = 0.7)
    circos.axis(h = "top", labels.cex = 0.8,
                labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
  })
  
  text(x = 0, y = -1.5, labels = label, cex = 1.5, col = "black", pos = 1)
  
  dev.off()
  circos.clear()
  invisible(file)
}

for (yr in names(periods)) {
  period <- periods[[yr]]
  dat <- filter(d0, year0 == as.integer(yr))
  if (nrow(dat) == 0) next
  file <- file.path("chord_africa", paste0("chord_africa_", period, ".png"))
  draw_africa_chord(dat, file, period)
  message("wrote ", file)
}
