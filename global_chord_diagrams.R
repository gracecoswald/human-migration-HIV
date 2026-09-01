# =====================================================================
# chord_diagrams.R
#
# Global chord diagrams of estimated regional migration flows, one per
# time period (Figure 1B and the equivalents for earlier periods), plus
# one pooled across all periods.
#
# Run AFTER migration_RMSD_creation.R, which supplies:
#   final_region_flow    - year0, orig_reg, dest_reg, total_flow
#   region_grouped_flow  - the same summed over all periods
#
# Flows are drawn in millions of individuals. Arrowheads give direction;
# arrow width at the base gives the size of the flow.
#
# Outputs: chord_diagrams/chord_<period>.png and chord_overall.png
# =====================================================================



# Packages are loaded by 00_libraries.R (see run_all.R).
stopifnot(exists("final_region_flow"), exists("region_grouped_flow"))

out_dir <- "chord_diagrams"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ---------------------------------------------------------------------
# Region order, colours and two-line labels
#   reg1 is the first label line, reg2 the second (NA for one-line names)
# ---------------------------------------------------------------------
d1 <- data.frame(
  region = c("caribbean", "latin_america", "north_america", "wce", "eeca",
             "india_nepal_sl", "southeast_asia", "east_asia", "oceania",
             "middle_east_north_africa", "west_africa", "east_africa",
             "eth_erit_dji", "central_africa", "southern_africa"),
  col1   = c("#40A4D8", "purple", "#33BEB7", "#E8579E", "green", "pink",
             "yellow", "#DB3937", "#C8A2C8", "#4B9CD3", "grey", "#B2CC24",
             "#FECC2F", "#FBA127", "#F66320"),
  reg1   = c("Caribbean", "Latin America", "North America", "West and Central",
             "Eastern Europe", "India, Nepal", "Southeast", "East", "Oceania",
             "Middle East", "West", "East", "Ethiopia, Eritrea", "Central",
             "Southern"),
  reg2   = c(NA, NA, NA, "Europe", "& Central Asia", "& Sri Lanka", "Asia",
             "Asia", NA, "& North Africa", "Africa", "Africa", "& Djibouti",
             "Africa", "Africa"),
  stringsAsFactors = FALSE
) %>%
  mutate(order1 = row_number())

stopifnot(setequal(union(final_region_flow$orig_reg, final_region_flow$dest_reg),
                   d1$region))

# Labels for each year0 bucket
period_labels <- c("1990" = "1990-1994", "1995" = "1995-1999",
                   "2000" = "2000-2004", "2005" = "2005-2009",
                   "2010" = "2010-2014", "2015" = "2015-2019",
                   "2020" = "2020-2022")

# ---------------------------------------------------------------------
# One chord diagram
# ---------------------------------------------------------------------
draw_chord <- function(dat, file, label = NULL) {
  png(file = file, height = 7, width = 7, units = "in", res = 500)
  
  circos.clear()
  par(mar = rep(0, 4), cex = 1)
  circos.par(start.degree = 90, track.margin = c(-0.1, 0.1),
             gap.degree = 4, points.overflow.warning = FALSE)
  
  chordDiagram(
    x = dat %>% select(orig_reg, dest_reg, total_flow),
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
  
  if (!is.null(label)) {
    text(x = 0, y = -1.5, labels = label, cex = 1.5, col = "black", pos = 1)
  }
  
  dev.off()
  circos.clear()
  invisible(file)
}

# ---------------------------------------------------------------------
# One diagram per time period
# ---------------------------------------------------------------------
d0 <- final_region_flow %>%
  filter(!is.na(orig_reg), !is.na(dest_reg)) %>%
  mutate(total_flow = total_flow / 1e06)   # millions of individuals

for (yr in sort(unique(d0$year0))) {
  lab  <- period_labels[[as.character(yr)]]
  file <- file.path(out_dir, paste0("chord_", lab, ".png"))
  draw_chord(filter(d0, year0 == yr), file, label = lab)
  message("wrote ", file)
}

# ---------------------------------------------------------------------
# Pooled across all periods
# ---------------------------------------------------------------------
d_all <- region_grouped_flow %>%
  filter(!is.na(orig_reg), !is.na(dest_reg)) %>%
  mutate(total_flow = total_flow / 1e06)

draw_chord(d_all, file.path(out_dir, "chord_overall.png"),
           label = "All periods")
message("wrote ", file.path(out_dir, "chord_overall.png"))
