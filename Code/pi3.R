folder_data <- file.path(FOLDER_PROYECTO, 'Data')
folder_pi3 <- file.path(FOLDER_PROYECTO, 'Output/pi3')

read_dta(file.path(folder_data, 'master_matching.dta')) %>% 
  write_parquet('pp.parquet')
df <- open_dataset('pp.parquet')
treatment_types <- df %>% select(starts_with('treated')) %>% names

treatment_type <- treatment_types[1]
for (treatment_type in treatment_types) {
  file <- sprintf('%s_matching.parquet', treatment_type)
  df %>% 
    group_by(!!sym(treatment_type), woman, age, semestre_grado, ESPECIALIDAD) %>%  
    summarise(n_ = n()) %>% 
    write_parquet(file.path(folder_pi3, file))
}


read_dta(file.path(folder_data, 'master_rethus.dta')) %>% 
  write_parquet('pp.parquet')
df <- open_dataset('pp.parquet')
treatment_types <- df %>% select(starts_with('treated')) %>% names

treatment_type <- treatment_types[1]
for (treatment_type in treatment_types) {
  file <- sprintf('%s.parquet', treatment_type)
  df %>% 
    mutate(treated = !!sym(treatment_type)) %>% 
    select(-!!sym(treatment_type)) %>% 
    filter(control == 1 | treated == 1) %>% collect %>% 
    replace_na(list(treated = F)) %>% 
    rename(!!sym(treatment_type) := treated) %>% 
    group_by(!!sym(treatment_type), ESPECIALIDAD, fechapregrado, date_start_especializacion) %>%  
    summarise(n_ = n()) %>% 
    write_parquet(file.path(folder_pi3, file))
}
