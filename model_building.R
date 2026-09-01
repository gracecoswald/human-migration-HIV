# =====================================================================
# model_building.R
#
# Fixed-effects quasi-Poisson models of RMSD on migration flow,
# prevalence difference and time.
#
# Run AFTER migration_RMSD_creation.R and migration_RMSD_adjusted.R.
#
# Outputs:
#   table2_main_results.docx   Table 2, main results
#   table4_standardised.docx   Appendix Table 4, standardised flow
#   predicted_RMSD_prev.png    Figure 4, predicted RMSD over time
#
# Sections
#   1. Load the three modelling datasets
#   2. Data checks
#   3. Functional form: Poisson, negative binomial, quasi-Poisson
#   4. Lag or no lag
#   5. Models
#   6. Tables 2 and 4
#   7. Model diagnostics
#   8. Predictions and Figure 4
#
# Standard errors are clustered by time period (cluster = "year0"),
# giving six clusters and t-tests on 5 degrees of freedom.
# =====================================================================

# Packages are loaded by 00_libraries.R (see run_all.R).

# =====================================================================
# 1. Load the modelling datasets
# =====================================================================
# raw (unweighted) migration flow
migration_RMSD <- read.csv("migration_RMSD.csv")

# flow weighted by destination population
migration_adj_RMSD <- read.csv("migration_adj_RMSD.csv")

# weighted flow plus absolute difference in HIV prevalence
migration_adj_RMSD_absdiff <- read.csv("migration_adj_RMSD_absdiff.csv")

# standardised flow, for comparing effect sizes across models
migration_RMSD             <- migration_RMSD %>%
  mutate(std_total_flow = scale(total_flow))
migration_adj_RMSD         <- migration_adj_RMSD %>%
  mutate(std_total_flow = scale(total_flow))
migration_adj_RMSD_absdiff <- migration_adj_RMSD_absdiff %>%
  mutate(std_total_flow = scale(total_flow))

# one-period lag of total flow, used in the lag comparison in section 4
migration_adj_RMSD_absdiff <- migration_adj_RMSD_absdiff %>%
  group_by(country_A, country_B) %>%
  arrange(year0) %>%
  mutate(lag_total_flow = dplyr::lag(total_flow, n = 1, order_by = year0)) %>%
  ungroup() %>%
  arrange(country_A, country_B)


# =====================================================================
# 2. Data checks
# =====================================================================
summary(migration_adj_RMSD_absdiff)
anyNA(migration_adj_RMSD_absdiff)

# collinearity between the numeric variables
numeric_vars <- migration_adj_RMSD_absdiff %>% select(where(is.numeric))
cor_matrix   <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45)

# distribution of the outcome
hist(migration_adj_RMSD_absdiff$RMSD)


# =====================================================================
# 3. Functional form
#
# Poisson, negative binomial and quasi-Poisson compared on the same
# specification. Quasi-Poisson is used throughout: RMSD is over-dispersed
# relative to Poisson, and the quasi likelihood handles that without
# assuming a count outcome.
# =====================================================================
poisdefault <- fixest::feglm(RMSD ~ total_flow + abs_diff_prev | year0 + big_region_pairs,
                             data = migration_adj_RMSD_absdiff,
                             family = poisson(link = "log"))

print(AIC(poisdefault))
print(BIC(poisdefault))

neg_bin <- alpaca::feglm.nb(RMSD ~ total_flow + abs_diff_prev | year0 + big_region_pairs,
                            data = migration_adj_RMSD_absdiff)

# AIC by hand: alpaca does not supply an AIC method
aic_nb_manual <- deviance(neg_bin) + 2 * length(coef(neg_bin))
print(aic_nb_manual)

quasi_pois <- fixest::feglm(RMSD ~ total_flow + abs_diff_prev | year0 + big_region_pairs,
                            data = migration_adj_RMSD_absdiff,
                            family = quasipoisson(link = "log"))

# dispersion parameter: > 1 indicates over-dispersion
residual_deviance <- deviance(quasi_pois)
phi <- residual_deviance / df.residual(quasi_pois)
print(residual_deviance)
print(phi)

# QAIC, the quasi-likelihood analogue of AIC
qaic_quasi <- deviance(quasi_pois) + 2 * length(coef(quasi_pois)) / phi
print(qaic_quasi)


# =====================================================================
# 4. Lag or no lag
#
# Compares QAIC with and without a one-period lag of total flow. The
# lagged term did not improve fit, so it is not used in the models below.
# =====================================================================
lagged_model <- fixest::feglm(RMSD ~ total_flow + lag_total_flow + abs_diff_prev | year0 + big_region_pairs,
                              data = migration_adj_RMSD_absdiff,
                              family = quasipoisson(link = "log"))

phi_lag  <- deviance(lagged_model) / df.residual(lagged_model)
qaic_lag <- deviance(lagged_model) + 2 * length(coef(lagged_model)) / phi_lag
print(qaic_lag)

# qaic_lag > qaic_quasi, so the lag is dropped


# =====================================================================
# 5. Models
#
#   1       unweighted flow, time FE
#   2       weighted flow, time FE
#   3-6     + prevalence difference and interactions, time FE
#   7-10    time + region-pair FE
#   11-14   time + country-pair FE
#   15-17   standardised flow (Appendix Table 4)
#
# fixef.rm = 'infinite_coef' drops only fixed-effect groups whose outcome
# is zero throughout; singleton pairs are retained.
# =====================================================================
# time fixed effects only
#total_flow unweighted
model_1 <- fixest::feglm(RMSD ~ total_flow |year0, 
                         data = migration_RMSD,
                         fixef.rm = 'infinite_coef',
                         cluster = "year0",
                         family = quasipoisson(link = "log"))
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


model_17 <- fixest::feglm(RMSD ~ std_total_flow + abs_diff_prev | year0,
                          data = migration_adj_RMSD_absdiff,
                          fixef.rm = 'infinite_coef',
                          cluster = "year0",
                          family = quasipoisson(link = "log"))

# =====================================================================
# 6. Tables
# =====================================================================
# Table 2, main results
modelsummary(
  list(model_1, model_2, model_3, model_7, model_10, model_11, model_14),
  statistic = "{std.error} (p = {p.value})",
  fmt       = fmt_statistic(estimate  = "%.2e",
                            std.error = "%.2e",
                            p.value   = "%.3f"),
  stars     = c('*' = .1, '**' = .05, '***' = .01),
  output    = "table2_main_results.docx"
)

# Appendix Table 4, standardised flow
modelsummary(
  list(model_15, model_16, model_17),
  statistic = "{std.error} (p = {p.value})",
  fmt       = fmt_statistic(estimate  = "%.2e",
                            std.error = "%.2e",
                            p.value   = "%.3f"),
  stars     = c('*' = .1, '**' = .05, '***' = .01),
  output    = "table4_standardised.docx"
)


# =====================================================================
# 7. Model diagnostics
# =====================================================================
pearson_resid  <- resid(model_14, type = "pearson")
deviance_resid <- resid(model_14, type = "deviance")
fitted_values  <- fitted(model_14)

par(mfrow = c(1, 2))
plot(fitted_values, pearson_resid, main = "Pearson Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Pearson Residuals")
abline(h = 0, col = "red")
plot(fitted_values, deviance_resid, main = "Deviance Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Deviance Residuals")
abline(h = 0, col = "red")
par(mfrow = c(1, 1))

pseudo_r2  <- 1 - deviance(model_17) / model_17$null.deviance
dispersion <- deviance(model_17) / df.residual(model_17)
print(pseudo_r2)
print(dispersion)

plot(residuals(model_17, type = "pearson"),  main = "Pearson Residuals")
plot(residuals(model_17, type = "deviance"), main = "Deviance Residuals")

# =====================================================================
# 8. Predictions and Figure 4
#
# Predictions from model_10.
#
# marginaleffects now refuses to build intervals when fixed effects are
# absorbed. This computes them directly, the way the older version did:
# the fixed effects enter the point prediction as known offsets, and the
# interval comes from the vcov of the estimated (non-FE) coefficients.
# No refit, so model_10 itself is untouched.
# =====================================================================

# Reference level for the region-pair fixed effect. Predictions are made
# holding it fixed; the LEVEL of predicted RMSD depends on which one, so
# state it in the caption.
ref_region <- migration_adj_RMSD_absdiff %>%
  count(big_region_pairs, sort = TRUE) %>% slice(1) %>% pull(big_region_pairs)
message("Predictions at big_region_pairs = ", ref_region)

grid <- expand.grid(
  total_flow    = c(1, 100, 1000, 2500),
  abs_diff_prev = c(0, 2.5, 5, 10),
  year0         = c(1990, 1995, 2000, 2005, 2010, 2015)
)
grid$big_region_pairs <- ref_region

# ---- Point predictions (fixest adds the fixed effects) ---------------
eta <- predict(model_10, newdata = grid, type = "link")

# ---- Delta-method SE from the estimated coefficients -----------------
b <- coef(model_10)
V <- vcov(model_10)

X <- cbind(
  "total_flow"                                 = grid$total_flow,
  "abs_diff_prev"                              = grid$abs_diff_prev,
  "total_flow:as.numeric(year0)"               = grid$total_flow * grid$year0,
  "total_flow:abs_diff_prev"                   = grid$total_flow * grid$abs_diff_prev,
  "as.numeric(year0):abs_diff_prev"            = grid$year0 * grid$abs_diff_prev,
  "total_flow:as.numeric(year0):abs_diff_prev" = grid$total_flow * grid$year0 * grid$abs_diff_prev
)

# fails loudly if the coefficient names differ from those assumed above
stopifnot(all(names(b) %in% colnames(X)))
X <- X[, names(b), drop = FALSE]

se_eta <- sqrt(rowSums((X %*% V) * X))

# 90% interval. qnorm matches what marginaleffects used; swap in
# qt(0.95, df = 5) to match the t(5) used for Table 2's p-values.
crit <- qnorm(0.95)

df_combined <- grid %>%
  mutate(estimate  = exp(eta),
         conf.low  = exp(eta - crit * se_eta),
         conf.high = exp(eta + crit * se_eta))

# sanity check before plotting — estimates should sit in the RMSD range
summary(df_combined$estimate)
summary(df_combined$conf.high - df_combined$conf.low)

# ---- Figure 4 --------------------------------------------------------
ggplot(df_combined, aes(x = year0, y = estimate,
                        colour = as.factor(abs_diff_prev),
                        fill   = as.factor(abs_diff_prev))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) +
  geom_line(linewidth = 1) +
  labs(
    title  = "Predicted RMSD Over Time with Confidence Intervals",
    x      = "Year",
    y      = "Predicted RMSD",
    colour = "Absolute difference in prevalence",
    fill   = "Absolute difference in prevalence"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ total_flow, scales = "fixed")

prev <- ggplot(df_combined, aes(x = year0, y = estimate,
                                colour = as.factor(total_flow),
                                fill   = as.factor(total_flow))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, colour = NA) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Predicted RMSD",
       colour = "Total flow", fill = "Total flow") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11),
    strip.text   = element_text(size = 14)
  ) +
  facet_wrap(~ abs_diff_prev, scales = "fixed", nrow = 3) +
  coord_cartesian(ylim = c(0, 0.3))

prev

ggsave("predicted_RMSD_prev.png", prev, width = 10, height = 15,
       units = "in", dpi = 300)