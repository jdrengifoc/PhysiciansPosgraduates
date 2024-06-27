
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
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_PILA/'
files <- list.files(folder, pattern = '2009|20[12][0-9]')

dict_path <- '../Data/PILA_dictionary.xlsx'
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
  sprintf('%s/data/treated_samples.parquet', private_folder)
) %>% 
  distinct(personabasicaid) %>% 
  collect %>% unlist %>% unname %>% sort

df <- NULL
tic()
for (file in files) {
  cat(paste('Began', file))
  tic()
  # Auxiliary variables to select proper names and get the desired types. 
  (df_selected <- filter(dict, uniname %in% selected_columns) %>% 
      select(uniname, uniclass, file) %>% drop_na(file) %>% 
      replace(is.na(.), ''))
  (selected_columns_file <- df_selected[[file]])
  (selected_columns1 <- df_selected$uniname)
  (desired_classes <- df_selected$uniclass)
  
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    # Select variables and rename.
    select(all_of(selected_columns_file)) %>%
    rename_at(vars(selected_columns_file), function(x) selected_columns1) %>%
    filter(personabasicaid %in% ids) %>%
    # Unify column types.
    mutate(
      across(all_of(selected_columns1[desired_classes == 'numeric']), as.numeric),
      across(all_of(selected_columns1[desired_classes == 'character']), as.character)
    ) %>% collect
  
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.

df %>% 
  # fechas
  mutate(
    fecha_pila = ymd(str_sub(fecha_cobertura, 1L, 10L)),
    year = str_sub(fecha_cobertura, 1L, 4L) %>% as.integer,
    month = str_sub(fecha_cobertura, 6L, 7L) %>% as.integer
    ) %>% select(-fecha_cobertura) %>% 
  write_parquet(sprintf('%s/data/history_PILA.parquet', private_folder))

sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


# To Stata ----------------------------------------------------------------
real_project_folder <- '../../PhysiciansPosgraduates/Data/new'
# RIPS
open_dataset(sprintf('%s/data/history_RIPS.parquet', private_folder)) %>% 
  collect %>% 
  write_dta(sprintf('%s/Merge_individual_RIPS.dta', real_project_folder))
# PILA
open_dataset(sprintf('%s/data/history_PILA.parquet', private_folder)) %>% 
  collect %>% 
  write_dta(sprintf('%s/P07_PILA_monthly.dta', real_project_folder))
# DESIGN
open_dataset('../New Methodology/PhysiciansPostgraduates/data/treated_samples.parquet') %>% 
  right_join(
    x = open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_demographics.parquet') %>% 
      rename_with(tolower, everything()) %>% select(personabasicaid, sexo)
  ) %>% 
  rename(
    rethus_sexo = sexo, fechapregrado = fecha_grado_pregrado
  ) %>% 
  collect %>% 
  write_dta(sprintf('%s/master_rethus.dta', real_project_folder))


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
