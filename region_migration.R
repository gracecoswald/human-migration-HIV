# =====================================================================
# region_flow_matrix.R
#
# Run AFTER migration_RMSD_creation.R, which supplies final_region_flow.
# =====================================================================


# total people coming/leaving each region


# Packages are loaded by 00_libraries.R (see run_all.R).
cross <- final_region_flow %>% filter(orig_reg != dest_reg)

immig <- cross %>%
  group_by(year0, region = dest_reg) %>%
  summarise(flow = sum(total_flow, na.rm = TRUE), .groups = "drop") %>%
  mutate(direction = "immigrants")

emig <- cross %>%
  group_by(year0, region = orig_reg) %>%
  summarise(flow = sum(total_flow, na.rm = TRUE), .groups = "drop") %>%
  mutate(direction = "emigrants")

within <- final_region_flow %>%
  filter(orig_reg == dest_reg) %>%
  group_by(year0, region = orig_reg) %>%
  summarise(flow = sum(total_flow, na.rm = TRUE), .groups = "drop") %>%
  mutate(direction = "within")

flow_table <- bind_rows(immig, emig, within) %>%
  mutate(direction = factor(direction,
                            levels = c("immigrants", "emigrants", "within"))) %>%
  complete(region, direction, year0, fill = list(flow = 0)) %>%
  arrange(region, direction) %>%
  pivot_wider(names_from = year0, values_from = flow, values_fill = 0)

flow_table

write.csv(flow_table, "total_region_migration_new.csv")

chk <- final_region_flow %>%
  group_by(year0) %>%
  summarise(
    diag  = sum(total_flow[orig_reg == dest_reg], na.rm = TRUE),
    off   = sum(total_flow[orig_reg != dest_reg], na.rm = TRUE),
    .groups = "drop"
  )

tbl_sum <- bind_rows(immig, emig, within) %>%
  group_by(year0, direction) %>%
  summarise(total = sum(flow), .groups = "drop") %>%
  pivot_wider(names_from = direction, values_from = total)

left_join(chk, tbl_sum, by = "year0") %>%
  mutate(within_ok = within - diag,      # should be 0
         immig_ok  = immigrants - off,   # should be 0
         emig_ok   = emigrants - off)    # should be 0

final_region_flow %>%
  filter(year0 == 1990,
         orig_reg %in% c("north_america", "latin_america"),
         dest_reg %in% c("north_america", "latin_america"),
         orig_reg != dest_reg)


# region flow matrix


region_order <- c("caribbean", "latin_america", "north_america", "wce", "eeca",
                  "india_nepal_sl", "southeast_asia", "east_asia", "oceania",
                  "middle_east_north_africa", "west_africa", "east_africa",
                  "eth_erit_dji", "central_africa", "southern_africa")

make_flow_matrix <- function(data, yr = NULL, regions = NULL, digits = 0) {
  d <- if (is.null(yr)) data else filter(data, year0 == yr)
  lv <- if (is.null(regions)) region_order else regions
  d %>%
    filter(orig_reg %in% lv, dest_reg %in% lv) %>%
    mutate(orig_reg = factor(orig_reg, levels = lv),
           dest_reg = factor(dest_reg, levels = lv)) %>%
    group_by(orig_reg, dest_reg) %>%
    summarise(flow = sum(total_flow, na.rm = TRUE), .groups = "drop") %>%
    complete(orig_reg, dest_reg, fill = list(flow = 0)) %>%
    arrange(orig_reg, dest_reg) %>%
    mutate(flow = round(flow, digits)) %>%
    pivot_wider(names_from = dest_reg, values_from = flow, values_fill = 0)
}

mat_2015 <- make_flow_matrix(final_region_flow, 2015)

mats <- final_region_flow$year0 %>%
  unique() %>% sort() %>%
  setNames(nm = .) %>%
  lapply(function(y) make_flow_matrix(final_region_flow, y))

mat_1990 <- mats[["1990"]]

m <- mat_2015 %>% tibble::column_to_rownames("orig_reg") %>% as.matrix()

tibble(
  region     = rownames(m),
  emigrants  = rowSums(m) - diag(m),
  immigrants = colSums(m) - diag(m),
  within     = diag(m)
)


ft <- mat_1990 %>%
  mutate(across(-orig_reg, ~ .x / 1e3)) %>%
  flextable() %>%
  colformat_double(digits = 1)

for (i in seq_along(region_order)) {
  ft <- bg(ft, i = i, j = i + 1, bg = "#E8E8E8")   # +1 for the label column
}

ft

mat_2015

write.csv(mat_2015, "2015_mig_matrix.csv")
