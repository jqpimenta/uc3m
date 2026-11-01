library(tidyverse)

# BOLSA FAMÍLIA, 01/2018-11/2021 ---------------------------------------------------------------------

# Paths
zip_folder <- "/Users/jii/Documents/LocalData/RawData/BolsaFamilia"
output_folder <- "/Users/jii/Documents/LocalData/BolsaFamilia_processed"
dir.create(output_folder, showWarnings = FALSE)

# Load correspondence table once
codigos <- read.csv("https://raw.githubusercontent.com/kelvins/municipios-brasileiros/main/csv/municipios.csv")

# Get all zip files
zip_files <- list.files(zip_folder, pattern = ".*_BolsaFamilia_Pagamentos\\.zip$", full.names = TRUE)

for (zip_path in zip_files) {
  
  # Extract date tag from filename (e.g. "201801")
  tag <- str_extract(basename(zip_path), "^[0-9]{6}")
  message("Processing: ", tag)
  
  # Unzip to a temp folder
  temp_dir <- file.path(zip_folder, "temp")
  unzip(zip_path, exdir = temp_dir)
  
  # Find the CSV inside
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
  
  # Process
  df <- read_csv2(csv_file, locale = locale(encoding = "Latin1")) %>%
    rename(
      uf = `UF`,
      cod_siafi = `CÓDIGO MUNICÍPIO SIAFI`,
      municipio = `NOME MUNICÍPIO`,
      valor = `VALOR PARCELA`
    ) %>%
    select(uf, cod_siafi, municipio, valor) %>%
    group_by(cod_siafi, municipio, uf) %>%
    summarise(avg_payment = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      cod_siafi = as.integer(cod_siafi),
      year_month = tag  # keep track of which month this is
    ) %>%
    left_join(codigos, by = c("cod_siafi" = "siafi_id")) %>%
    select(year_month, cod_siafi, municipio, uf, avg_payment, latitude, longitude)
  
  # Save processed CSV
  write_csv(df, file.path(output_folder, paste0(tag, "_processed.csv")))
  
  # Delete unzipped file to save space
  unlink(temp_dir, recursive = TRUE)
  
  message("Done: ", tag, " — ", nrow(df), " municipalities")
}

# Merge all processed CSVs at the end
all_files <- list.files(output_folder, pattern = "\\.csv$", full.names = TRUE)

df_all <- all_files %>%
  map(read_csv) %>%
  bind_rows()
write_csv(df_all, file.path(output_folder, "bolsafamilia_all.csv"))
message("All done! Total rows: ", nrow(df_all))

# AUXÍLIO BRASIL, 11/2021-02/2023 --------------------------------------------------------------------

# Paths
zip_folder <- "/Users/jii/Documents/LocalData/RawData/BolsaFamilia"
output_folder <- "/Users/jii/Documents/LocalData/AuxilioBrasil_processed"
dir.create(output_folder, showWarnings = FALSE)

# Load correspondence table once
codigos <- read.csv("https://raw.githubusercontent.com/kelvins/municipios-brasileiros/main/csv/municipios.csv")

# Get all zip files
zip_files <- list.files(zip_folder, pattern = ".*_AuxilioBrasil\\.zip$", full.names = TRUE)

for (zip_path in zip_files) {
  
  # Extract date tag from filename (e.g. "201801")
  tag <- str_extract(basename(zip_path), "^[0-9]{6}")
  message("Processing: ", tag)
  
  # Unzip to a temp folder
  temp_dir <- file.path(zip_folder, "temp")
  unzip(zip_path, exdir = temp_dir)
  
  # Find the CSV inside
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
  
  # Process
  df <- read_csv2(csv_file, locale = locale(encoding = "Latin1")) %>%
    rename(
      uf = `UF`,
      cod_siafi = `CÓDIGO MUNICÍPIO SIAFI`,
      municipio = `NOME MUNICÍPIO`,
      valor = `VALOR PARCELA`
    ) %>%
    select(uf, cod_siafi, municipio, valor) %>%
    group_by(cod_siafi, municipio, uf) %>%
    summarise(avg_payment = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      cod_siafi = as.integer(cod_siafi),
      year_month = tag  # keep track of which month this is
    ) %>%
    left_join(codigos, by = c("cod_siafi" = "siafi_id")) %>%
    select(year_month, cod_siafi, municipio, uf, avg_payment, latitude, longitude)
  
  # Save processed CSV
  write_csv(df, file.path(output_folder, paste0(tag, "_processed.csv")))
  
  # Delete unzipped file to save space
  unlink(temp_dir, recursive = TRUE)
  
  message("Done: ", tag, " — ", nrow(df), " municipalities")
}

# Merge all processed CSVs at the end
all_files <- list.files(output_folder, pattern = "\\.csv$", full.names = TRUE)

df_all <- all_files %>%
  map(read_csv) %>%
  bind_rows()
write_csv(df_all, file.path(output_folder, "auxiliobrasil_all.csv"))
message("All done! Total rows: ", nrow(df_all))

# NOVO BOLSA FAMÍLIA, 03/2023-03/2026 ----------------------------------------------------------------

# Paths
zip_folder <- "/Users/jii/Documents/LocalData/RawData/BolsaFamilia"
output_folder <- "/Users/jii/Documents/LocalData/BolsaFamilia_processed"
dir.create(output_folder, showWarnings = FALSE)

# Load correspondence table once
codigos <- read.csv("https://raw.githubusercontent.com/kelvins/municipios-brasileiros/main/csv/municipios.csv")

# Get all zip files
zip_files <- list.files(zip_folder, pattern = ".*_NovoBolsaFamilia\\.zip$", full.names = TRUE)

for (zip_path in zip_files) {
  
  # Extract date tag from filename (e.g. "201801")
  tag <- str_extract(basename(zip_path), "^[0-9]{6}")
  message("Processing: ", tag)
  
  # Unzip to a temp folder
  temp_dir <- file.path(zip_folder, "temp")
  unzip(zip_path, exdir = temp_dir)
  
  # Find the CSV inside
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
  
  # Process
  df <- read_csv2(csv_file, locale = locale(encoding = "Latin1")) %>%
    rename(
      uf = `UF`,
      cod_siafi = `CÓDIGO MUNICÍPIO SIAFI`,
      municipio = `NOME MUNICÍPIO`,
      valor = `VALOR PARCELA`
    ) %>%
    select(uf, cod_siafi, municipio, valor) %>%
    group_by(cod_siafi, municipio, uf) %>%
    summarise(avg_payment = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      cod_siafi = as.integer(cod_siafi),
      year_month = tag  # keep track of which month this is
    ) %>%
    left_join(codigos, by = c("cod_siafi" = "siafi_id")) %>%
    select(year_month, cod_siafi, municipio, uf, avg_payment, latitude, longitude)
  
  # Save processed CSV
  write_csv(df, file.path(output_folder, paste0(tag, "_processed.csv")))
  
  # Delete unzipped file to save space
  unlink(temp_dir, recursive = TRUE)
  
  message("Done: ", tag, " — ", nrow(df), " municipalities")
}

# Merge all processed CSVs at the end
all_files <- list.files(output_folder, pattern = "\\.csv$", full.names = TRUE)

df_all <- all_files %>%
  map(read_csv) %>%
  bind_rows()
write_csv(df_all, file.path(output_folder, "novobolsafamilia_all.csv"))
message("All done! Total rows: ", nrow(df_all))

# MERGE ALL DATA -------------------------------------------------------------------------------------

df_final <- bind_rows(
  read_csv("/Users/jii/Documents/LocalData/BolsaFamilia_processed/bolsafamilia_all.csv"),
  read_csv("/Users/jii/Documents/LocalData/AuxilioBrasil_processed/auxiliobrasil_all.csv"),
  read_csv("/Users/jii/Documents/LocalData/NovoBolsaFamilia_processed/novobolsafamilia_all.csv")
)

state_lookup <- tibble(
  uf     = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  state = c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara",
             "Distrito Federal", "Espirito Santo", "Goias", "Maranhao",
             "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Para",
             "Paraiba", "Parana", "Pernambuco", "Piaui", "Rio de Janeiro",
             "Rio Grande do Norte", "Rio Grande do Sul", "Rondonia", "Roraima",
             "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins")
)

df_final <- df_final %>%
  left_join(state_lookup, by = "uf") %>%
  mutate(municipality = str_to_title(municipio)) %>%
  select(-uf, -municipio, -cod_siafi) %>%
  mutate(
    year  = as.integer(substr(as.character(year_month), 1, 4)),
    month = as.integer(substr(as.character(year_month), 5, 6)),
    date  = ym(year_month)
  )
write_csv(df_final, "/Users/jii/Documents/LocalData/bolsafamilia_complete.csv")

# Check total rows and date range
nrow(df_final)
range(df_final$year_month)

# Make sure there are no gaps
df_final %>%
  distinct(year_month) %>%
  arrange(year_month) %>%
  print(n = Inf)
