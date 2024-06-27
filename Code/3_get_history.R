source('Code/requirements.R')
library(dataRC)

# Get RIPS history --------------------------------------------------------

folder <- file.path(FOLDER_DATOS, '_RIPS')
files <- list.files(folder, pattern = 'proc|urg|hos|cons', recursive = T)

dict_path <- file.path(FOLDER_DATOS, 'unidicts/RIPS_dictionary.xlsx')
dict <- read_excel(dict_path, sheet = 'colnames')
names(dict) <- sapply(str_split(names(dict), '\\.'),
                      function(x) paste(tail(x, 2), collapse = '.'))

# MODIFY: SELECT THE DESIRED COLUMNS (USE THE UNINAME OF THE DICTIONARY).
selected_columns <- c('PERSONABASICAID', 'DATE_JUAN',
                      'DIAG_PRIN', 'DIAG_R1', 'COD_DIAG_R2', 'COD_DIAG_R3',
                      'CAUSA_EXTERNA')
ids <- open_dataset(file.path(FOLDER_PROYECTO, 'Data/treated_samples.parquet')) %>% 
  distinct(personabasicaid) %>% 
  collect %>% unlist %>% unname %>% sort

df <- NULL
tic()
for (file in files) {
  cat(paste('Began', file))
  tic()
  
  df0 <- open_dataset(file.path(folder, file)) %>%
    dataRC::unify_colnames(dict, file, selected_columns) %>% 
    
    # MODIFY: PROCESS THE DATA AS NEEDED.
    filter(PERSONABASICAID %in% ids) %>%
    mutate(SERVICE = str_sub(
      str_match(file, 'cons|proc|hosp|urg'), end = 1L)) %>% 
    
    dataRC::unify_classes(dict, file, selected_columns) %>% 
    collect
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.
df %>% 
  dataRC::relocate_columns(selected_columns) %>% 
  mutate(DATE = ymd(str_sub(DATE_JUAN, 1L, 10L))) %>% select(-DATE_JUAN) %>% 
  mutate(across(
    all_of(selected_columns[str_detect(selected_columns, 'DIAG')]), ~toupper(.)
  )) %>%
  # Stata equivalent.
  rename(DIAG_R2 = COD_DIAG_R2, DIAG_R3 = COD_DIAG_R3) %>% 
  rename_with(tolower, everything()) %>% 
  write_parquet(file.path(FOLDER_PROYECTO, 'Data/history_RIPS.parquet'))
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat
 
# Get PILA history --------------------------------------------------------
tic()
folder <- file.path(FOLDER_DATOS, '_PILA')
files <- list.files(folder, pattern = '2009|20[12][0-9]')

dict_path <- file.path(FOLDER_DATOS, 'unidicts/PILA_dictionary.xlsx')
dict <- read_excel(dict_path, sheet = 'colnames')
names(dict) <- sapply(str_split(names(dict), '\\.'),
                      function(x) paste(tail(x, 2), collapse = '.'))

# MODIFY: SELECT THE DESIRED COLUMNS (USE THE UNINAME OF THE DICTIONARY).
selected_columns <- c(
  'personabasicaid', 'id', 'ibc_ccf', 'ibc_pens', 'ibc_rprof', 'ibc_salud',
  'fecha_cobertura', 'salario_bas', 'sal_dias_cot', 'tipo_cotiz', 'sexomode',
  'fechantomode', 'ciudad_cod', 'depto_cod', 'licen_mat', 'incap_gral',
  'incap_trab')

ids <- open_dataset(
  file.path(FOLDER_PROYECTO, 'Data/treated_samples.parquet')
) %>% 
  distinct(personabasicaid) %>% 
  collect %>% unlist %>% unname %>% sort

df <- NULL
tic()
for (file in files) {
  cat(paste('Began', file))
  tic()
  
  df0 <- open_dataset(file.path(folder, file)) %>%
    dataRC::unify_colnames(dict, file, selected_columns) %>% 
    
    # MODIFY: PROCESS THE DATA AS NEEDED.
    filter(personabasicaid %in% ids) %>%
    
    dataRC::unify_classes(dict, file, selected_columns) %>%
    collect
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.
df %>% 
  dataRC::relocate_columns(selected_columns) %>% 
  mutate(
    fecha_pila = ymd(str_sub(fecha_cobertura, 1L, 10L)),
    year = str_sub(fecha_cobertura, 1L, 4L) %>% as.integer,
    month = str_sub(fecha_cobertura, 6L, 7L) %>% as.integer
    ) %>% select(-fecha_cobertura) %>% 
  write_parquet(file.path(FOLDER_PROYECTO, 'Data/history_PILA.parquet'))

sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat

# salario -----------------------------------------------------------------

# df_aux <- open_dataset(sprintf('%s/data/history_PILA.parquet', private_folder)) %>% 
#   # Nuevas variables.
#   rename(tipo_cotizante = tipo_cotiz) %>% 
#   mutate(
#     pila_independientes = tipo_cotizante %in% c(2, 3, 16, 41, 42, 59, 57, 66),
#     pila_posgrado_salud = tipo_cotizante == 21,
#     pila_dependientes = !pila_independientes & !pila_posgrado_salud
#   )
# df_aux %>% distinct(personabasicaid, id, year, month) %>% collect %>% nrow
# df_aux %>% collect %>% nrow
# 
# # Ingreso: maximo salario de dependiente o en su defecto como independiente.
# # Los estudiantes de especialidad no tienen salario (en esas cotizaciones).
# df_aux1 <- df_aux %>% 
#   filter(pila_dependientes | pila_independientes) %>% 
#   group_by(personabasicaid, id, year, month, pila_dependientes, pila_independientes) %>% 
#   summarise(
#     across(matches('^(salario_bas|ibc_)'), ~ max(., na.rm = T))
#   ) %>% ungroup
# 
# df_aux1 %>% 
#   left_join(
#     df_aux1 %>% 
#       group_by(personabasicaid, id, year, month) %>% 
#       summarise(
#         any_dependiente = any(pila_dependientes), 
#         any_independiente = any(pila_independientes)
#       )
#   ) %>% collect %>% View
#   group_by(personabasicaid, id, year, month) %>% collect %>% 
#   mutate(
#     across(
#       matches('^(salario_bas|ibc_)'),
#       ~ any_dependiente * pila_dependientes * . + 
#         !any_dependiente * pila_independientes * .
#     )
#   ) %>% 
#   summarise(
#     across(matches('^(salario_bas|ibc_)'), ~ max(., na.rm = T))
#   ) %>% 
#  
