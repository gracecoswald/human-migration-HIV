# =====================================================================
# pie_charts.R
#
# Country-level and region-level HIV-1 variant pie charts, and the
# variant colour legends, using the 19-category collapsed scheme from
# the manuscript.
#
# Run AFTER subtyped_new.R and region_distributions.R, which write:
#   proportions.csv                 (country x period)
#   proportions_region_time.csv     (region x period)
#
# NOTE ON other_CRF
#   This scheme plots CRF01_AE, CRF02_AG and CRF07_BC as their own
#   slices, so other_CRF must exclude all three. It is derived here from
#   the proportion columns rather than read from the file, so the script
#   does not depend on how other_CRF is defined in subtyped_new.R.
#
# Outputs:
#   pie_charts_country/<country>_<period>.png
#   pie_charts_region/<region>_<period>.png
#   Subtype_Legend.pdf, Small_Subtype_Legend.pdf
# =====================================================================


# ---------------------------------------------------------------------
# 1. Collapsed variant scheme and colours (as in the manuscript legend)
# ---------------------------------------------------------------------

# Packages are loaded by 00_libraries.R (see run_all.R).
subtype_levels <- c("A", "B", "C", "D", "F", "G", "H", "J", "K", "L",
                    "N", "O", "P", "CRF01_AE", "CRF02_AG", "CRF07_BC",
                    "other_CRF", "URFs", "unspecified_recombinants")

subtype_totals <- paste0(subtype_levels, "_adjusted")

main_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "red", "#d64918",
                 "#8c564b", "#e377c2", "#bcbd22", "#17becf",
                 "#aec7e8", "#ffbb78", "#ff9896", "#c5b0d5", "hotpink",
                 "#9467bd", "#7f7f4f", "navy", "yellow", "darkgrey")
names(main_colors) <- subtype_totals

color_mapping <- main_colors

color_df <- data.frame(
  Subtype = sub("_adjusted$", "", subtype_totals),
  Column  = subtype_totals,
  Color   = unname(color_mapping),
  stringsAsFactors = FALSE
)

# Display labels for the legend
display_label <- c(unspecified_recombinants = "unspec. recomb.",
                   other_CRF                = "other CRF")

# ---------------------------------------------------------------------
# 2. Proportions over the collapsed scheme
#
# Read from the proportions files. other_CRF is not converted to a
# proportion by the upstream scripts, so it is derived here as
#   total_CRF - (CRF01_AE + CRF02_AG + CRF07_BC)
# using columns that are already proportions. That makes this independent
# of how other_CRF is defined in subtyped_new.R, and guarantees the 19
# categories sum to one:
#   A..P + total_CRF + URFs + unspecified = total_recorded_genotypes_adj
# ---------------------------------------------------------------------
plotted <- setdiff(subtype_totals, "other_CRF_adjusted")
needed  <- c(plotted, "total_CRF_adjusted")

# Upstream write.csv calls omit row.names = FALSE, leaving an unnamed
# index column that dplyr cannot handle.
read_props <- function(path) {
  d <- read.csv(path, check.names = FALSE)
  keep <- !is.na(names(d)) & nzchar(names(d)) & names(d) != "X"
  d[, keep, drop = FALSE]
}

to_proportions <- function(props, id_cols) {
  missing <- setdiff(needed, names(props))
  if (length(missing)) stop("Not in file: ", paste(missing, collapse = ", "))
  
  out <- props %>%
    mutate(other_CRF_adjusted = total_CRF_adjusted -
             (CRF01_AE_adjusted + CRF02_AG_adjusted + CRF07_BC_adjusted)) %>%
    dplyr::select(all_of(id_cols), all_of(subtype_totals))
  
  neg <- rowSums(out[, subtype_totals] < -1e-9, na.rm = TRUE) > 0
  if (any(neg)) warning(sum(neg), " rows have a negative other_CRF")
  
  chk <- rowSums(out[, subtype_totals], na.rm = TRUE)
  bad <- abs(chk - 1) > 1e-6 & chk > 0
  if (any(bad)) {
    warning(sum(bad), " of ", length(chk),
            " rows do not sum to 1 (range ", round(min(chk[bad]), 4), " to ",
            round(max(chk[bad]), 4), ")")
  }
  out
}

countries_props <- read_props("proportions.csv")
regions_props   <- read_props("proportions_region_time.csv")

prop_long <- to_proportions(countries_props,
                            c("year_category", "Site 1: Country")) %>%
  pivot_longer(all_of(subtype_totals), names_to = "Subtype",
               values_to = "Proportion") %>%
  mutate(Subtype = factor(Subtype, levels = subtype_totals))

prop_long_regions <- to_proportions(regions_props,
                                    c("year_category", "region")) %>%
  pivot_longer(all_of(subtype_totals), names_to = "Subtype",
               values_to = "Proportion") %>%
  mutate(Subtype = factor(Subtype, levels = subtype_totals))

# ---------------------------------------------------------------------
# 3. Pie chart, transparent background, no legend
#    ggplot rather than plotly/webshot, which needs PhantomJS
# ---------------------------------------------------------------------
generate_pie_chart <- function(df, name, period, dir) {
  # plot_ly orders pie slices by descending value and sweeps
  # anticlockwise from twelve o'clock (its default direction). Re-level
  # per chart and use direction = -1 to match. Colours are keyed by name
  # in color_mapping, so they follow the subtype regardless of order.
  df <- df %>%
    filter(!is.na(Proportion), Proportion > 0) %>%
    arrange(desc(Proportion)) %>%
    mutate(Subtype = factor(as.character(Subtype), levels = as.character(Subtype)))
  
  p <- ggplot(df, aes(x = "", y = Proportion, fill = Subtype)) +
    geom_col(width = 1, colour = NA, position = position_stack(reverse = TRUE)) +
    coord_polar(theta = "y", direction = -1) +
    scale_fill_manual(values = color_mapping) +
    theme_void() +
    theme(legend.position  = "none",
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  safe <- gsub("[^A-Za-z0-9_.-]", "_", name)
  file <- file.path(dir, paste0(safe, "_", period, ".png"))
  ggsave(file, p, width = 3, height = 3, dpi = 100, bg = "transparent")
  file
}

draw_all <- function(long_df, key, dir) {
  if (!dir.exists(dir)) dir.create(dir)
  files <- list()
  for (nm in unique(long_df[[key]])) {
    sub_nm <- long_df %>% filter(.data[[key]] == nm)
    for (period in unique(sub_nm$year_category)) {
      d <- sub_nm %>% filter(year_category == period)
      if (nrow(d) == 0 || sum(d$Proportion, na.rm = TRUE) == 0) next
      files[[length(files) + 1]] <- data.frame(
        name = nm, year_category = period,
        file = generate_pie_chart(d, nm, period, dir),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, files)
}

country_pie_files <- draw_all(prop_long, "Site 1: Country", "pie_charts_country")
region_pie_files  <- draw_all(prop_long_regions, "region", "pie_charts_region")

message("country charts: ", nrow(country_pie_files),
        " | region charts: ", nrow(region_pie_files))

# ---------------------------------------------------------------------
# 4. Legends
# ---------------------------------------------------------------------
swatch <- function(df, i, title_size) {
  lab <- df$Subtype[i]
  lab <- ifelse(lab %in% names(display_label), display_label[[lab]], lab)
  ggplot() +
    geom_tile(aes(x = 1, y = 1), fill = df$Color[i], width = 0.5, height = 0.5) +
    theme_void() +
    theme(plot.title = element_text(size = title_size, hjust = 0.5, vjust = 0.5)) +
    ggtitle(lab)
}

# Full legend, all 19 variants
plots <- lapply(seq_len(nrow(color_df)), function(i) swatch(color_df, i, 8))
title <- textGrob("HIV-1 Variant Legend", gp = gpar(fontsize = 14, fontface = "bold"))

pdf("Subtype_Legend.pdf", width = 8.27, height = 11.69)   # A4
grid.arrange(grobs = plots, ncol = 4, top = title)
dev.off()
