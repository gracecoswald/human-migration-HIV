# =====================================================================
# regional_maps.R
#
# Regional migration maps with country HIV-1 variant pie charts, one per
# time period, for the five regions in the manuscript:
#
#   north_south_america   North and South America
#   europe_africa         Europe and Africa
#   europe_namerica       Europe and North America
#   europe                Europe and Central Asia
#   asia                  Asia and Oceania
#
# Run AFTER:
#   migration_RMSD_creation.R  - supplies net_flow
#   pie_chart_generation.R     - supplies country_pie_files
#
# Outputs: regional_maps/<region>_<period>.png
# =====================================================================



# Packages are loaded by 00_libraries.R (see run_all.R).
source("ne_country_names.R")   # country_name_corrections, ne_name()

stopifnot(exists("net_flow"), exists("country_pie_files"))

if (!dir.exists("regional_maps")) dir.create("regional_maps")

periods <- c("1990" = "1990-1994", "1995" = "1995-1999", "2000" = "2000-2004",
             "2005" = "2005-2009", "2010" = "2010-2014", "2015" = "2015-2019")

world <- ne_countries(scale = "medium", returnclass = "sf")

# ---------------------------------------------------------------------
# Map definitions
#
#   subset      how to pick the polygons from `world`
#   areas       plot_area values kept, for both origin and destination
#   min_flow    smallest flow drawn
#   xlim/ylim   coord_sf extent (ylim NULL = full range of the subset)
#   arrow_mult  multiplier on the arrow head length
#   size_range  line width range
#   pie_half    half-width of each pie chart, in degrees
# ---------------------------------------------------------------------
europe_subregions  <- c("Western Europe", "Northern Europe",
                        "Southern Europe", "Eastern Europe")

map_defs <- list(
  
  north_south_america = list(
    subset     = quote(region_wb %in% c("Latin America & Caribbean", "North America")),
    areas      = c("Latin America and the Caribbean", "North America"),
    min_flow   = 3000,
    xlim = c(-150, -40), ylim = NULL,
    arrow_mult = 0.8, size_range = c(0.05, 0.85), pie_half = 2.5),
  
  europe_africa = list(
    subset     = quote(subregion %in% c("Eastern Africa", "Southern Europe",
                                        "Western Africa", "Northern Europe",
                                        "Eastern Europe", "Northern Africa",
                                        "Western Europe", "Southern Africa",
                                        "Middle Africa")),
    areas      = c("Europe", "East Europe & Central Asia",
                   "Sub-Saharan Africa", "North Africa"),
    min_flow   = 5000,
    xlim = c(-50, 100), ylim = NULL,
    arrow_mult = 0.8, size_range = c(0.05, 0.85), pie_half = 2.0),
  
  europe_namerica = list(
    subset     = quote(subregion %in% c(europe_subregions, "Northern America")),
    areas      = c("Europe", "East Europe & Central Asia", "North America"),
    min_flow   = 5000,
    xlim = c(-130, 100), ylim = c(25, 70),
    arrow_mult = 0.8, size_range = c(0.05, 0.85), pie_half = 2.0),
  
  europe = list(
    subset     = quote(subregion %in% c(europe_subregions, "Central Asia")),
    areas      = c("Europe", "East Europe & Central Asia"),
    min_flow   = 5000,
    xlim = c(-25, 100), ylim = c(30, 70),
    arrow_mult = 0.8, size_range = c(0.05, 0.85), pie_half = 1.5),
  
  asia = list(
    subset     = quote(subregion %in% c("Western Asia", "South-Eastern Asia",
                                        "Melanesia", "Micronesia", "Polynesia",
                                        "Eastern Asia", "Southern Asia",
                                        "Australia and New Zealand")),
    areas      = c("East Asia", "South-East Asia", "South Asia",
                   "West Asia", "Oceania"),
    min_flow   = 5000,
    xlim = c(20, 180), ylim = NULL,
    arrow_mult = 0.8, size_range = c(0.05, 0.85), pie_half = 2.5)
)

# Legend breaks, as in the published figures
size_breaks  <- c(5000, 10000, 50000, 100000, 250000, 500000, 1000000)
size_labels  <- comma(size_breaks)
alpha_breaks <- log10(size_breaks) * 0.1
alpha_labels <- comma(size_breaks)

arrow_length <- function(flow) log10(flow) * 0.15

# ---------------------------------------------------------------------
# One map
# ---------------------------------------------------------------------
make_map <- function(def, yr, period, region_name) {
  
  poly <- world %>% filter(eval(def$subset))
  
  coords <- suppressWarnings(poly %>% st_centroid() %>% st_coordinates())
  country_coords <- data.frame(name = poly$name,
                               lon = coords[, 1], lat = coords[, 2])
  
  migration_data <- net_flow %>%
    filter(plot_area_origin %in% def$areas,
           plot_area_dest   %in% def$areas,
           year0 == as.integer(yr),
           net_flow_pbclosed >= def$min_flow) %>%
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
    mutate(arrow_len = arrow_length(net_flow_pbclosed) * def$arrow_mult,
           alpha     = log10(net_flow_pbclosed) * 0.1)
  
  base_map <- ggplot() +
    geom_sf(data = poly, fill = "lightgray", color = "black") +
    geom_curve(data = migration_data,
               aes(x = origin_lon, y = origin_lat,
                   xend = dest_lon, yend = dest_lat,
                   size = net_flow_pbclosed, alpha = alpha),
               arrow = arrow(length = unit(migration_data$arrow_len, "cm"),
                             type = "open"),
               color = "blue", curvature = 0.2, lineend = "round") +
    scale_size_continuous(name = "Flow Size", breaks = size_breaks,
                          labels = size_labels, range = def$size_range) +
    scale_alpha_continuous(name = "Flow Size", breaks = alpha_breaks,
                           labels = alpha_labels, range = c(0.1, 1)) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 12),
      legend.key.size  = unit(1, "lines"),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank()
    ) +
    coord_sf(xlim = def$xlim, ylim = def$ylim, expand = FALSE)
  
  # Pie charts for the countries on this map, this period
  pies <- country_pie_files %>%
    rename(country = name) %>%
    mutate(country = ne_name(country)) %>%
    filter(year_category == period) %>%
    inner_join(country_coords, by = c("country" = "name"))
  
  final_map <- base_map
  for (i in seq_len(nrow(pies))) {
    final_map <- final_map +
      annotation_custom(
        rasterGrob(readPNG(pies$file[i]), interpolate = TRUE),
        xmin = pies$lon[i] - def$pie_half, xmax = pies$lon[i] + def$pie_half,
        ymin = pies$lat[i] - def$pie_half, ymax = pies$lat[i] + def$pie_half
      )
  }
  
  file <- file.path("regional_maps",
                    paste0(region_name, "_", period, ".png"))
  ggsave(file, plot = final_map, width = 20, height = 15,
         units = "in", dpi = 300)
  message("wrote ", file, "  (", nrow(migration_data), " flows, ",
          nrow(pies), " pies)")
  invisible(file)
}

# ---------------------------------------------------------------------
# All regions, all periods
# ---------------------------------------------------------------------
for (region_name in names(map_defs)) {
  def <- map_defs[[region_name]]
  for (yr in names(periods)) {
    make_map(def, yr, periods[[yr]], region_name)
  }
}

# ---------------------------------------------------------------------
# Which plot_area values exist, for checking the `areas` above
# ---------------------------------------------------------------------
# sort(unique(c(net_flow$plot_area_origin, net_flow$plot_area_dest)))
