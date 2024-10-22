setwd('PhysiciansPosgraduates')
source('Code/requirements.R')


# Minimalist --------------------------------------------------------------
# Parameters.
rethus_download_date <- as.Date('2023-03-05')

# Read data.
df_RIPS <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('^history_RIPS.parquet', full.names = T) %>% open_dataset()
df_PILA <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('^history_PILA.parquet', full.names = T) %>% open_dataset()
df_ReTHUS <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('ReTHUS_demographics', full.names = T) %>% open_dataset()

# get ids.
ids_PILA <- df_PILA %>% distinct(personabasicaid) %>% collect() %>% 
  pull(personabasicaid)
ids_RIPS <- df_RIPS %>% distinct(personabasicaid) %>% collect() %>% 
  pull(personabasicaid)
ids <- union(ids_RIPS, ids_PILA)

# initiallize file.
file_demographics <- file.path(FOLDER_PROYECTO, 'demographics.parquet')
tibble(personabasicaid = ids) %>% write_parquet(file_demographics)

# Create age from PILA's birth date.
df_edad_pila <- df_PILA %>% 
  filter(!is.na(fechantomode)) %>% distinct(personabasicaid, fechantomode) %>% 
  mutate(age = year(rethus_download_date) - year(as.Date(fechantomode)))

# Normal PILA's age values. To guarantee ReTHUS' age quality.
min_age_PILA <- df_edad_pila %>% summarise(min(age, na.rm = T)) %>% collect %>% pull
max_age_PILA <- df_edad_pila %>% summarise(max(age, na.rm = T)) %>% collect %>% pull

open_dataset(file_demographics) %>% 
  # Get PILA's SEX
  left_join(
    df_PILA %>% filter(sexomode <= 1) %>% distinct(personabasicaid, sexomode),
    by = 'personabasicaid'
  ) %>% 
  # Get ReTHUS sex and use it to fill NA in PILA's age.
  left_join(
    df_ReTHUS %>% 
      select(personabasicaid = PERSONABASICAID, SEXO) %>% 
      filter(SEXO %in% c('FEMENINO', 'MASCULINO')) %>% 
      mutate(SEXO = if_else(SEXO == 'FEMENINO', 0, 1)),
    by = 'personabasicaid'
  ) %>% mutate(sex = coalesce(sexomode, SEXO)) %>% 
  # Merge PILA's age.
  left_join(df_edad_pila, by = 'personabasicaid') %>% 
  # Fill PILA's NA ages with ReTHUS' ages in the PILA's age range.
  left_join(
    df_ReTHUS %>% select(personabasicaid = PERSONABASICAID, EDAD) %>% 
      filter(between(EDAD, min_age_PILA, max_age_PILA)),
    by = 'personabasicaid'
  ) %>% 
  mutate(age = coalesce(age, EDAD)) %>% 
  # Delete individuals with missing demographic attributes.
  filter(!is.na(age), !is.na(sex)) %>% 
  select(personabasicaid, sex, age) %>% 
  write_parquet(file_demographics)




# personabasicaid ---------------------------------------------------------

ids_PILA <- df_PILA %>% distinct(personabasicaid) %>% collect() %>% 
  pull(personabasicaid)

ids_RIPS <- df_RIPS %>% distinct(personabasicaid) %>% collect() %>% 
  pull(personabasicaid)

ids_ReTHUS <- df_ReTHUS %>% distinct(PERSONABASICAID) %>% collect() %>% 
  pull(PERSONABASICAID)

all(intersect(ids_RIPS, ids_ReTHUS) == ids_RIPS)
all(intersect(ids_PILA, ids_ReTHUS) == ids_PILA)
ids <- union(ids_RIPS, ids_PILA)

file_demographics <- file.path(FOLDER_PROYECTO, 'demographics.parquet')
tibble(personabasicaid = ids) %>% write_parquet(file_demographics)

# SEXO --------------------------------------------------------------------

df_ReTHUS %>% group_by(SEXO, EDAD) %>% summarise(n_ = n()) %>% ungroup() %>% 
  collect %>% write_parquet('Output/Tables/sexo_edad_rethus.parquet')

open_dataset('Output/Tables/sexo_edad_rethus.parquet') %>% 
  group_by(SEXO) %>% summarise(n_ = sum(n_)) %>% collect

df_sexo_pila <- df_PILA %>% filter(sexomode <= 1) %>%
  group_by(personabasicaid, sexomode) %>% summarise(n_ = n())

# No hay diferentes sexomode por persona
df_sexo_pila %>% collect %>% nrow
df_sexo_pila %>% distinct(personabasicaid) %>% collect %>% nrow
# Faltan algunos ¿podemos llenar con ReTHUS?
ids %>% length

# PILA y ReTHUS se validan entre si.
df_sexo <- df_ReTHUS %>% 
  select(personabasicaid = PERSONABASICAID, SEXO) %>% 
  filter(SEXO %in% c('FEMENINO', 'MASCULINO')) %>% 
  mutate(SEXO = if_else(SEXO == 'FEMENINO', 0, 1)) %>% 
  left_join(df_sexo_pila,  by = 'personabasicaid')

df_sexo %>% 
  mutate(validation = SEXO == sexomode) %>% 
  group_by(validation) %>% summarise(n()) %>% 
  collect

df <- open_dataset(file_demographics) %>% 
  left_join(
    df_PILA %>% filter(sexomode <= 1) %>% distinct(personabasicaid, sexomode),
    by = 'personabasicaid'
    ) %>% 
  left_join(
    df_ReTHUS %>% 
      select(personabasicaid = PERSONABASICAID, SEXO) %>% 
      filter(SEXO %in% c('FEMENINO', 'MASCULINO')) %>% 
      mutate(SEXO = if_else(SEXO == 'FEMENINO', 0, 1)),
    by = 'personabasicaid'
  ) %>% mutate(sex = coalesce(sexomode, SEXO))

df %>% mutate(validation = sexomode == SEXO) %>% 
  summarise(mean(validation, na.rm = T)) %>% collect

# EDAD PILA ----------------------------------------------------------------

# Some errors (7 years old & > 90).
open_dataset('Output/Tables/sexo_edad_rethus.parquet') %>% 
  group_by(EDAD) %>% summarise(n_ = sum(n_)) %>% arrange(EDAD) %>% 
  collect %>% View


df_fechanto_pila <- df_PILA %>% filter(!is.na(fechantomode)) %>% 
  distinct(personabasicaid, fechantomode) 

df_fechanto_pila %>% collect %>% nrow
ids_PILA %>% length




# EDAD ReTHUS -------------------------------------------------------------

rethus_download_date <- as.Date('2023-03-05')
# The correction seems to be unneeded.
df_edad_pila <- df_fechanto_pila %>% 
  mutate(fechantomode = as.Date(fechantomode)) %>% 
  mutate(age = year(rethus_download_date) - year(fechantomode),
         correction_age = -1 * as.integer(
           (month(rethus_download_date) > month(fechantomode)) |
             ((month(rethus_download_date) == month(fechantomode)) & 
                (day(rethus_download_date) > day(fechantomode)))
         )) #%>% 
  # mutate(age = age + correction_age) %>% select(-correction_age)

# Exactly one age per id.
df_edad_pila %>% distinct(personabasicaid) %>% collect %>% nrow
df_edad_pila %>% distinct(personabasicaid, age) %>% collect %>% nrow
# Normal age values. Better quality than ReTHUS
df_edad_pila %>% 
  group_by(age) %>% summarise(n_ = n()) %>% arrange(age) %>% collect %>% View
df_edad_pila %>% summarise(max(age, na.rm = T), min(age, na.rm = T)) %>% collect

min_age_PILA <- df_edad_pila %>% summarise(min(age, na.rm = T)) %>% collect %>% pull
max_age_PILA <- df_edad_pila %>% summarise(max(age, na.rm = T)) %>% collect %>% pull
# However almost everything is correct.
df_ReTHUS %>% 
  select(personabasicaid = PERSONABASICAID, EDAD) %>% 
  left_join(df_edad_pila, by = 'personabasicaid') %>% 
  mutate(error = EDAD - age) %>% 
  group_by(error) %>% summarise(n_ = n()) %>% 
  arrange(error) %>% collect %>% View
  
df %>% 
  left_join(df_edad_pila, by = 'personabasicaid') %>% 
  left_join(
    # Fill with ReTHUS only ages between PILA age range.
    df_ReTHUS %>% select(personabasicaid = PERSONABASICAID, EDAD) %>% 
      filter(between(EDAD, min_age_PILA, max_age_PILA)),
    by = 'personabasicaid'
    ) %>% 
  mutate(age = coalesce(age, EDAD)) %>% 
  collect %>% 
  filter(!is.na(age), !is.na(sex)) %>% 
  select(personabasicaid, sex, age) %>% 
  write_parquet(file_demographics)
