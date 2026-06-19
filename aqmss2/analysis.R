library(tidyverse)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(spdep)
library(stringi)
library(broom)

panel <- read_csv("/Users/jii/Documents/LocalData/bf_finalpanel.csv")

# LPM models ----------------------------------------------------------------------------------------

m1 <- feols(violent_event ~ log_payment | muni_norm + time_id, 
  data = panel, cluster = ~muni_norm)

m2 <- feols(violent_event ~ log_payment * rel_deprivation | muni_norm + time_id, 
  data = panel, cluster = ~muni_norm)

summary(m1)
summary(m2)

# Marginal effects

coefs <- coef(m2)
vcv <- vcov(m2)

beta_pay <- coefs["log_payment"]
beta_int <- coefs["log_payment:rel_deprivation"]
cov_pi <- vcv["log_payment", "log_payment:rel_deprivation"]
var_pay <- vcv["log_payment", "log_payment"]
var_int <- vcv["log_payment:rel_deprivation", "log_payment:rel_deprivation"]

rd_grid <- seq(
  quantile(panel$rel_deprivation, .05, na.rm = TRUE),
  quantile(panel$rel_deprivation, .95, na.rm = TRUE),
  length.out = 100
)

me_df <- tibble(
  rd = rd_grid,
  ME = beta_pay + beta_int * rd_grid,
  SE = sqrt(var_pay + rd_grid^2 * var_int + 2 * rd_grid * cov_pi),
  lo = ME - 1.96 * SE,
  hi = ME + 1.96 * SE
)

# Poisson models ------------------------------------------------------------------------------------

mP1 <- fepois(event_count ~ log_payment | muni_norm + time_id, 
  data = panel, cluster = ~muni_norm)

mP2 <- fepois(event_count ~ log_payment * rel_deprivation | muni_norm + time_id, 
  data = panel, cluster = ~muni_norm)

summary(mP1)
summary(mP2)

coef(mP1)["log_payment"]
exp(coef(mP1)["log_payment"])
coef(mP2)["log_payment"]
exp(coef(mP2)["log_payment"])

# Marginal effects

coefs <- coef(mP2)
vcv <- vcov(mP2)

beta_pay <- coefs["log_payment"]
beta_int <- coefs["log_payment:rel_deprivation"]
cov_pi <- vcv["log_payment", "log_payment:rel_deprivation"]
var_pay <- vcv["log_payment", "log_payment"]
var_int <- vcv["log_payment:rel_deprivation", "log_payment:rel_deprivation"]

rd_grid <- seq(
  quantile(panel$rel_deprivation, .05, na.rm = TRUE),
  quantile(panel$rel_deprivation, .95, na.rm = TRUE),
  length.out = 100
)

me_df_p <- tibble(
  rd  = rd_grid,
  ME  = exp(beta_pay + beta_int * rd_grid),
  SE  = sqrt(var_pay + rd_grid^2 * var_int + 2 * rd_grid * cov_pi),
  lo  = exp(beta_pay + beta_int * rd_grid - 1.96 * SE),
  hi  = exp(beta_pay + beta_int * rd_grid + 1.96 * SE)
)

# Tables --------------------------------------------------------------------------------------------

# LPM models
modelsummary(
  list("Baseline" = m1, "Interaction" = m2), 
  stars  = TRUE, 
  output = "aqmss2/tables/lpm.tex"
)

# Poisson models
modelsummary(
  list("Baseline" = mP1, "Interaction" = mP2), 
  stars  = TRUE, 
  output = "aqmss2/tables/poisson.tex"
)

# Descriptives
panel %>%
  rename(
    `CCT Payment` = log_payment,
    `Relative Deprivation` = rel_deprivation
  ) %>%
  datasummary(
    `CCT Payment` + `Relative Deprivation` ~ N + Mean + SD + Median + Min + Max,
    data   = .,
    fmt    = 2,
    output = "aqmss2/tables/descriptives.tex"
  )

# Plots ---------------------------------------------------------------------------------------------

# LPM interaction

ggplot(me_df, aes(x = rd, y = ME)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15) +
  geom_line() +
  geom_rug(
    data = distinct(panel, state, rel_deprivation), 
    aes(x = rel_deprivation), 
    inherit.aes = FALSE, 
    sides = "b", alpha = .5
  ) +
  coord_cartesian(xlim = c(-450, 350)) +
  ## Because of Distrito Federal making the plot less readable
  labs(x = "Relative Deprivation", y = "Marginal Effect") +
  theme_minimal()

ggsave("aqmss2/figures/me_lpm.pdf")

# Poisson interaction

ggplot(me_df_p, aes(x = rd, y = ME)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15) +
  geom_line() +
  geom_rug(
    data = distinct(panel, state, rel_deprivation), 
    aes(x = rel_deprivation), 
    inherit.aes = FALSE, 
    sides = "b", alpha = .5
  ) +
  coord_cartesian(xlim = c(-450, 350)) +
  ## Because of Distrito Federal making the plot less readable
  labs(x = "Relative Deprivation", y = "Incidence Rate Ratio") +
  theme_minimal()

ggsave("aqmss2/figures/me_poisson.pdf")

# Diagnostics ---------------------------------------------------------------------------------------

# Fitted value check (LPM validity)

fitted_vals <- fitted(m2)

round(mean(fitted_vals < 0 | fitted_vals > 1), 4)
min(fitted_vals)
max(fitted_vals)

# Nested FE comparison (justifying two-way FE)

m2_pooled <- feols(violent_event ~ log_payment * rel_deprivation,
                      data = panel, cluster = ~muni_norm)
m2_unit_only <- feols(violent_event ~ log_payment * rel_deprivation | muni_norm,
                      data = panel, cluster = ~muni_norm)
m2_time_only <- feols(violent_event ~ log_payment * rel_deprivation | time_id,
                      data = panel, cluster = ~muni_norm)

etable(m2_pooled, m2_unit_only, m2_time_only, m2,
       headers = c("Pooled", "Unit FE", "Time FE", "Two-Way FE"),
       fitstat = c("r2", "wr2", "n"),
       title   = "Nested model comparison — justifying two-way FE")

summary(m2_pooled)
summary(m2_unit_only)
summary(m2_time_only)

# Moran's I (spatial autocorrelation in residuals)

normalise <- function(x) {
  x %>% str_trim() %>% str_to_upper() %>% stri_trans_general("Latin-ASCII")
}

bf_coords <- read_csv("/Users/jii/Documents/LocalData/bolsafamilia_complete.csv") %>%
  mutate(muni_norm = normalise(municipality)) %>%
  group_by(muni_norm) %>%
  summarise(lat = first(latitude), lon = first(longitude))

kept_rows <- obs(m2)
m2_resid  <- residuals(m2)

stopifnot(length(kept_rows) == length(m2_resid))

resid_muni <- panel[kept_rows, ] %>%
  mutate(.resid = m2_resid) %>%
  group_by(muni_norm) %>%
  summarise(mean_resid = mean(.resid, na.rm = TRUE)) %>%
  left_join(bf_coords, by = "muni_norm") %>%
  filter(!is.na(lat), !is.na(mean_resid))

coords_mat <- matrix(
  c(as.numeric(resid_muni$lon), as.numeric(resid_muni$lat)),
  ncol = 2
)
nb <- knn2nb(knearneigh(coords_mat, k = 5))
lw <- nb2listw(nb, style = "W")

moran.test(resid_muni$mean_resid, lw)
