source('Code/requirements.R')
library(dataRC)
folder <- file.path(FOLDER_PROYECTO, 'Data')

# RIPS
read_parquet(file.path(folder, 'history_RIPS.parquet')) %>% 
  write_dta(file.path(folder, 'Merge_individual_RIPS.dta'))
# PILA
read_parquet(file.path(folder, 'history_PILA.parquet')) %>% 
  write_dta(file.path(folder, 'P07_PILA_monthly.dta'))
# DESIGN
open_dataset(file.path(folder, 'Data/treated_samples.parquet')) %>% 
  right_join(
    x = open_dataset(file.path(folder, 'RETHUS_demographics.parquet')) %>% 
      rename_with(tolower, everything()) %>% select(personabasicaid, sexo)
  ) %>% 
  rename(
    rethus_sexo = sexo, fechapregrado = fecha_grado_pregrado
  ) %>% 
  collect %>% 
  write_dta(file.path(folder, 'master_rethus.dta'))