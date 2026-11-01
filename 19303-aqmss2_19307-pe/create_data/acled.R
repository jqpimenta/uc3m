library(tidyverse)
library(lubridate)

path_acled <- "/Users/jii/Documents/LocalData/RawData/ACLEDData_2026-05-05.csv"
path_bf <- "/Users/jii/Documents/LocalData/bolsafamilia_complete.csv"
path_out <- "/Users/jii/Documents/LocalData/acled_clean.csv"

# Filter data ---------------------------------------------------------------------------------------

acled <- read_csv(path_acled, show_col_types = FALSE)
bf <- read_csv(path_bf, show_col_types = FALSE)

violent_subtypes <- c(
  "Violent demonstration", 
  "Mob violence", 
  "Protest with intervention", 
  "Excessive force against protesters"
)

acled_vm <- acled %>%
  filter(sub_event_type %in% violent_subtypes)

print(count(acled_vm, sub_event_type))

# Restrict data range -------------------------------------------------------------------------------

acled_vm <- acled_vm %>%
  mutate(
    event_date = ymd(event_date), 
    year = year(event_date),  
    month = month(event_date)
  ) %>%
  filter(event_date >= ymd("2018-01-01"), event_date <= ymd("2026-03-01"))

# Normalise municipality names ----------------------------------------------------------------------

normalise <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII")
}

acled_vm <- acled_vm %>%
  mutate(muni_norm = normalise(admin2))
bf <- bf %>%
  mutate(muni_norm = normalise(municipality))

acled_munis <- unique(acled_vm$muni_norm)
bf_munis <- unique(bf$muni_norm)

matched <- intersect(acled_munis, bf_munis)
unmatched <- setdiff(acled_munis, bf_munis)

print(unmatched)

# Manual corrections of municipality names ----------------------------------------------------------

manual_map <- c(
  "AUGUSTO SEVERO"       = "CAMPO GRANDE (EX-AUGUSTO SEVERO)",
  "DISTRITO FEDERAL"     = "BRASILIA",
  "DONA EUZEBIA"         = "DONA EUSEBIA",
  "FERNANDO DE NORONHA"  = "DISTRITO ESTADUAL DE FERNANDO DE NORONHA",
  "PARATY"               = "PARATI",
  "SANTA IZABEL DO PARA" = "SANTA ISABEL DO PARA"
)

acled_vm <- acled_vm %>%
  mutate(muni_norm = recode(muni_norm, !!!manual_map))

unmatched_post <- setdiff(unique(acled_vm$muni_norm), bf_munis)
if (length(unmatched_post) > 0) print(unmatched_post)

# Build panel ---------------------------------------------------------------------------------------

acled_agg <- acled_vm %>%
  group_by(muni_norm, year, month) %>%
  summarise(
    event_count = n(), 
    fatalities = sum(fatalities, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(violent_event = 1L)

all_munis <- bf %>%
  distinct(muni_norm, municipality, state)
all_months <- bf %>%
  distinct(year, month)

full_panel <- crossing(all_munis, all_months)

acled_clean <- full_panel %>%
  left_join(
    acled_agg %>%
      select(muni_norm, year, month, event_count, fatalities, violent_event), 
    by = c("muni_norm", "year", "month")
  ) %>%
  mutate(
    event_count = replace_na(event_count, 0L),
    fatalities = replace_na(fatalities, 0L),
    violent_event = replace_na(violent_event, 0L)
  )

write_csv(acled_clean, path_out)
