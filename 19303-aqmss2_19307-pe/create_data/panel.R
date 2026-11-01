library(tidyverse)
library(stringi)
library(readxl)

select <- dplyr::select

path_acled <- "/Users/jii/Documents/LocalData/acled_clean.csv"
path_bf <- "/Users/jii/Documents/LocalData/bolsafamilia_complete.csv"
path_income <- "/Users/jii/Documents/LocalData/RawData/income_state.xlsx"
path_out <- "/Users/jii/Documents/LocalData/bf_finalpanel.csv"

acled <- read_csv(path_acled, show_col_types = FALSE)
bf <- read_csv(path_bf, show_col_types = FALSE)

normalise <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII")
}

bf <- bf %>%
  mutate(muni_norm = normalise(municipality))

# Build panel ---------------------------------------------------------------------------------------

panel <- acled %>%
  left_join(
    select(bf, muni_norm, state, year, month, avg_payment), 
    by = c("muni_norm", "state", "year", "month")
  ) %>%
  mutate(
    log_payment = log(avg_payment), 
    time_id = (year - 2018) * 12 + month
  )

# Compute relative deprivation ----------------------------------------------------------------------

national_avg_income <- (774.72 + 814.30) / 2

income_state <- read_xlsx(path_income) %>%
  rename(state = Territorialidades) %>%
  select(state, matches("Renda per capita")) %>%
  filter(!state %in% c("Brasil", " ", ""), !is.na(state)) %>%
  mutate(
    income_avg = rowMeans(across(matches("Renda per capita")), na.rm = TRUE), 
    rel_deprivation = income_avg - national_avg_income, 
    state = str_trim(state) %>%
      stri_trans_general("Latin-ASCII") %>%
      str_to_title()
  ) %>%
  mutate(
    state = case_match(
      state, 
      "Rio Grande Do Norte" ~ "Rio Grande do Norte", 
      "Rio Grande Do Sul" ~ "Rio Grande do Sul", 
      "Mato Grosso Do Sul" ~ "Mato Grosso do Sul", 
      "Rio De Janeiro" ~ "Rio de Janeiro", 
      .default = state
    )
  ) %>%
  select(state, income_avg, rel_deprivation)

panel <- panel %>%
  left_join(income_state, by = "state")

stopifnot(
  "State name mismatch — check income_state$state against panel$state" = 
    sum(is.na(panel$rel_deprivation)) == 0
)

write_csv(panel, path_out)
