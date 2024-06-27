# Get PILA history --------------------------------------------------------
library(dataRC)
tic()
folder <- '//wmedesrv/gamma/Christian Posso/_banrep_research/datos_originales/_PILA/'
files <- list.files(folder, pattern = '200[89]|20[12][0-9]')

dict_path <- '//wmedesrv/gamma/Christian Posso/_banrep_research/datos_originales/unidicts/PILA_dictionary.xlsx'
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
  
  df0 <- open_dataset(sprintf('%s/%s', folder, file)) %>%
    dataRC::unify_colnames(dict, file, selected_columns) %>% 
    filter(personabasicaid %in% ids) %>%
    dataRC::unify_classes(dict, file, selected_columns) %>% 
    collect
  
  df <- bind_rows(df, df0)
  
  sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
}
# MODIFY: PROCESS AND SAVE THE REQUIRED DATA.
df %>% 
  mutate(fecha_cobertura = ymd(str_sub(fecha_cobertura, 1L, 10L))) %>% 
  write_parquet('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet')
sprintf('\n\t Completed in %f mins\n', get_values_tic_msg('min')) %>% cat