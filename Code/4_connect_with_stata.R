source('Code/requirements.R')

folder <- file.path(FOLDER_PROYECTO, 'Data')

# RIPS
read_parquet(file.path(folder, 'history_RIPS.parquet')) %>% 
  write_dta(file.path(folder, 'Merge_individual_RIPS.dta'))
# PILA
read_parquet(file.path(folder, 'history_PILA.parquet')) %>% 
  write_dta(file.path(folder, 'history_PILA.dta'))
# DESIGN
open_dataset(file.path(folder, 'treated_samples.parquet')) %>% 
  right_join(
    x = open_dataset(file.path(folder, 'RETHUS_demographics.parquet')) %>% 
      rename_with(tolower, everything()) %>% select(personabasicaid, sexo)
  ) %>% 
  rename(
    rethus_sexo = sexo, fechapregrado = fecha_grado_pregrado
  ) %>% 
  collect %>% 
  write_dta(file.path(folder, 'master_rethus.dta'))


# matching ----------------------------------------------------------------
df_samples <- open_dataset(file.path(folder, 'treated_samples.parquet')) %>% 
  right_join(
    x = open_dataset(file.path(folder, 'RETHUS_demographics.parquet')) %>% 
      rename_with(tolower, everything()) %>% select(personabasicaid, sexo)
  ) %>% 
  rename(
    rethus_sexo = sexo, fechapregrado = fecha_grado_pregrado
  )


df <- open_dataset(file.path(FOLDER_PROYECTO, 'demographics.parquet')) %>% 
  left_join(
    open_dataset(file.path(FOLDER_PROYECTO, 'Data', 'treated_samples.parquet')) %>% 
      select(personabasicaid, fecha_grado_pregrado, starts_with('treated_'), control),
    by = 'personabasicaid'
  ) %>%
  mutate(
    semestre_grado = year(fecha_grado_pregrado) + 
      0.5 * semester(fecha_grado_pregrado)
  ) %>% collect

df_new <- NULL
treatment_types <- df %>% select(starts_with('treated_')) %>% names
treatment_type <- treatment_types[2]

for (treatment_type in treatment_types) {
  
  df_treatement <- df %>% 
    mutate(treated = !!sym(treatment_type)) %>% 
    filter(control | treated) %>% 
    replace_na(list(treated = F)) %>% print
  
  # Get range of fecha_grado
  min_treated_fecha_grado_pregrado <- df_treatement %>% 
    filter(treated) %>% 
    summarise(min(semestre_grado)) %>% pull %>% print
  max_treated_fecha_grado_pregrado <- df_treatement %>% 
    filter(treated) %>% 
    distinct(semestre_grado) %>% 
    summarise(max(semestre_grado)) %>% pull %>% print
  df_treatement <- df_treatement %>% 
    filter(
      between(semestre_grado, 
              min_treated_fecha_grado_pregrado,
              max_treated_fecha_grado_pregrado)
    )
  
  df_matching <- df_treatement %>% 
    select(personabasicaid, treated, woman, age, semestre_grado)
  
  df0 <- df_treatement %>% 
    left_join(
      df_matching %>% filter(treated) %>% rename(id_treated = personabasicaid) %>% 
        left_join(df_matching %>% filter(!treated), 
                  by = c('woman', 'age', 'semestre_grado'), 
                  relationship = 'many-to-many') %>% 
        distinct(personabasicaid) %>% mutate(control_matching = T),
      by = 'personabasicaid'
    ) %>% 
    filter(control_matching | treated) %>%
    select(personabasicaid, woman, age, semestre_grado, 
           !!sym(treatment_type) := treated)
  
  if (is.null(df_new)) {
    df_new <- df0
  } else {
    df_new <- df_new %>% 
      left_join(df0, by = c('personabasicaid', 'woman', 'age', 'semestre_grado'))
  }
}
df_new %>% summarise(across(starts_with('treated_'), ~sum(!is.na(.))))

df_new %>% left_join(
  df_samples %>% select(-starts_with('treated_'), -control) %>% collect,
  by = 'personabasicaid') %>% 
  write_dta(file.path(folder, 'master_matching.dta'))

