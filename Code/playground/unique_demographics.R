setwd('PhysiciansPosgraduates')
source('Code/requirements.R')
file.path(FOLDER_PROYECTO, 'Data') %>% list.files
file <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('ReTHUS_demographics', full.names = T)
open_dataset(file) %>% 
  group_by(SEXO, EDAD) %>% summarise(n_ = n()) %>% ungroup() %>% 
  collect %>% write_parquet('Output/Tables/sexo_edad_rethus.parquet')

open_dataset('Output/Tables/sexo_edad_rethus.parquet') %>% 
  group_by(SEXO) %>% 
  summarise(n_ = sum(n_)) %>% collect


open_dataset('Output/Tables/sexo_edad_rethus.parquet') %>% 
  group_by(EDAD) %>% 
  summarise(n_ = sum(n_)) %>% arrange(EDAD) %>% collect


df_sexo_pila <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('^history_PILA.parquet', full.names = T) %>% 
  open_dataset() %>% 
  filter(sexomode <= 1) %>%
  group_by(personabasicaid, sexomode) %>% summarise(n_ = n())
df_sexo_pila %>% 
  collect %>% nrow

df_fechanto_pila <- file.path(FOLDER_PROYECTO, 'Data') %>% 
  list.files('^history_PILA.parquet', full.names = T) %>% 
  open_dataset() %>%
  filter(!is.na(fechantomode)) %>% 
  distinct(personabasicaid, fechantomode) 
df_fechanto_pila %>% 
  collect %>% nrow

df_sexo <- open_dataset(file) %>% 
  select(PERSONABASICAID, SEXO) %>% 
  filter(SEXO %in% c('FEMENINO', 'MASCULINO')) %>% 
  mutate(SEXO = if_else(SEXO == 'FEMENINO', 0, 1)) %>% 
  left_join(df_sexo_pila,  by = join_by(PERSONABASICAID==personabasicaid))
  
df_sexo %>% 
  mutate(validation = SEXO == sexomode) %>% 
  group_by(validation) %>% summarise(n()) %>% 
  collect

df_sexo %>% 
  filter(is.na(SEXO)) %>% collect %>% nrow



rethus_download_date <- as.Date('2023-03-05')
df_edad_pila <- df_fechanto_pila %>% 
  mutate(fechantomode = as.Date(fechantomode)) %>% 
  mutate(age = year(rethus_download_date) - year(fechantomode),
         correction_age = -1 * as.integer(
           (month(rethus_download_date) > month(fechantomode)) |
             ((month(rethus_download_date) == month(fechantomode)) & 
                (day(rethus_download_date) > day(fechantomode)))
         )) #%>% 
  # mutate(age = age + correction_age) %>% select(-correction_age)

df_edad_pila %>% distinct(personabasicaid) %>% collect %>% nrow
df_edad_pila %>% distinct(personabasicaid, age) %>% collect %>% nrow

df_edad <- open_dataset(file) %>% 
  select(PERSONABASICAID, EDAD) %>% 
  left_join(df_edad_pila, by = join_by(PERSONABASICAID==personabasicaid))
df_edad %>% 
  collect
