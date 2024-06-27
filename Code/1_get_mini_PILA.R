# Get PILA history --------------------------------------------------------
tic()
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/_PILA/'
files <- list.files(folder, pattern = '200[89]|20[12][0-9]')

dict_path <- '../Data/PILA_dictionary.xlsx'
dict <- read_excel(dict_path, sheet = 'colnames')
names(dict) <- sapply(str_split(names(dict), '\\.'),
                      function(x) paste(tail(x, 2), collapse = '.'))

# MODIFY: SELECT THE DESIRED COLUMNS (USE THE UNINAME OF THE DICTIONARY).
selected_columns <- c('personabasicaid', 'fecha_cobertura', 'tipo_cotiz')
ids <- open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_demographics.parquet') %>% 
  distinct(PERSONABASICAID) %>% collect %>% unlist %>% unname

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
  mutate(fecha_cobertura = ymd(str_sub(fecha_cobertura, 1L, 10L))) %>% 
  write_parquet('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet')
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat


