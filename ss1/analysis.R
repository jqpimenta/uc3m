# Packages and data ---------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(modelsummary)
library(ggplot2)
library(gridExtra)
library(interactions)

df <- read_dta("/Users/jii/Downloads/ZA8833_v1-0-0.dta/ZA8833_v1-0-0.dta")

# Data cleaning -------------------------------------------------------------------------------

df_final <- df %>%
  select(
    pa01, ## LINKS-RECHTS-SELBSTEINSTUFUNG, BEFR.
    id02, ## SUBJEKTIVE SCHICHTEINSTUFUNG, BEFR.
    id01, ## GERECHTER ANTEIL A.LEBENSSTANDARD,BEFR.? 
    vg10, ## STREBE DANACH RISIKEN ZU VERMEIDEN
    sex, ## GESCHLECHT, BEFRAGTE(R) 
    age, ## ALTER: BEFRAGTE(R)
    educ, ## ALLGEMEINER SCHULABSCHLUSS
    incc ## NETTOEINKOMMEN(OFFENE+LISTENANGABE),KAT.
  ) %>%
  mutate(across(everything(), ~ ifelse(. < 0, NA, .))) %>%
  drop_na()

df_final$id01_c <- scale(df_final$id01, center = TRUE, scale = FALSE)
df_final$id02_c <- scale(df_final$id02, center = TRUE, scale = FALSE)

# Data analysis -------------------------------------------------------------------------------

# Descriptives

table(df_final$sex)
prop.table(table(df_final$sex)) * 100

table(df_final$educ)
prop.table(table(df_final$educ)) * 100

mean(df_final$age, na.rm = TRUE)
sd(df_final$age, na.rm = TRUE)
range(df_final$age, na.rm = TRUE)

# Main effect

main <- lm(
  pa01 ~ id01 + vg10 + sex + age + educ + incc, 
  data = df_final
)
summary(main)

# Interaction effects

interaction1 <- lm(
  pa01 ~ (id01 * id02) + sex + age + educ + incc, 
  data = df_final
)
summary(interaction1)

interaction1_c <- lm(
  pa01 ~ (id01_c * id02_c) + sex + age + educ + incc, 
  data = df_final
)
summary(interaction1_c)

interaction2 <- lm(
  pa01 ~ (vg10 * id02) + sex + age + educ + incc, 
  data = df_final
)
summary(interaction2)

interaction2_c <- lm(
  pa01 ~ (vg10 * id02_c) + sex + age + educ + incc, 
  data = df_final
)
summary(interaction2_c)

# Robust SE -----------------------------------------------------------------------------------

library(lmtest)
library(sandwich)

coeftest(main, vcov = vcovHC(main, type = "HC3"))
coeftest(interaction1_c, vcov = vcovHC(interaction1_c, type = "HC3"))
coeftest(interaction2_c, vcov = vcovHC(interaction2_c, type = "HC3"))

# Tables --------------------------------------------------------------------------------------

modelsummary(
  list(
    "Main" = main, 
    "Interaction (Relative Deprivation)" = interaction1_c, 
    "Interaction (Risk Aversion)" = interaction2_c
  ),
  stars = TRUE,
  vcov = "robust",  
  coef_rename = c(
    "id02"  = "Class",
    "id01"  = "Relative Deprivation",
    "id02_c" = "Class, centred",
    "id01_c" = "Relative Reprivation, centred",
    "vg10"  = "Risk Aversion",
    "sex"  = "Sex",
    "age" = "Age",
    "educ" = "Education",
    "incc"  = "Income"
  ),
  output  = "ss1/tables/coefficients.tex"
)

# Plots ---------------------------------------------------------------------------------------

# Main effect

reldepr <- ggplot(df_final, aes(x = factor(id01), y = pa01)) +
  geom_boxplot() +
  labs(x = "Left/Right Placement",
       y = "Relative Deprivation") +
  theme_minimal()

riskav <- ggplot(df_final, aes(x = factor(vg10), y = pa01)) +
  geom_boxplot() +
  labs(x = "Left/Right Placement",
       y = "Risk Aversion") +
  theme_minimal()

ggsave(
  "ss1/figures/main.pdf", 
  plot = arrangeGrob(reldepr, riskav, ncol = 1),
  width = 14, height = 12
)

# Interaction effects

reldepr_int <- interact_plot(interaction1_c, 
  pred = id01_c, 
  modx = id02_c, 
  legend.main = "Class",
  x.label = "Relative Deprivation",
  y.label = "Left/Right Placement") +
  theme_minimal()

riskav_int <- interact_plot(interaction2_c, 
  pred = vg10, 
  modx = id02_c, 
  legend.main = "Class",
  x.label = "Risk Aversion",
  y.label = "Left/Right Placement") +
  theme_minimal()

ggsave(
  "ss1/figures/inter.pdf", 
  plot = arrangeGrob(reldepr_int, riskav_int, ncol = 1),
  width = 14, height = 12
)

# Diagnostics ---------------------------------------------------------------------------------

# Breusch-Pagan Test
library(lmtest)
bptest(main)
bptest(interaction1)
bptest(interaction2)

# Variance Inflation Factors
library(car)
vif(main)
vif(interaction1)
vif(interaction2)
vif(interaction1_c)
vif(interaction1_c)

# Shapiro-Wilk Test & Q-Q Plots
shapiro.test(residuals(main))
shapiro.test(residuals(interaction1))
shapiro.test(residuals(interaction2))

plot(main, which = 2)
plot(interaction1, which = 2)
plot(interaction2, which = 2)
## Although the Shapiro-Wilk test was statistically significant, the Q-Q plots inspection showed 
## that there are no substantial deviations

# Durbin-Watson Test
dwtest(main)
dwtest(interaction1)
dwtest(interaction2)

# Cook's Distance
plot(main, which = 4)
plot(interaction1, which = 4)
plot(interaction2, which = 4)
# Given the large sample size, the influential cases were retained
