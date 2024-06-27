
# Readable ----------------------------------------------------------------
df_rethus <- open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  group_by(PERSONABASICAID) %>% 
  # Pregrado nacional, tiene especialidad
  summarise(
    pregrado_nacional = any(PREGRADO & !EXTRANJERO),
    ESPECIALIDAD = any(ESPECIALIDAD),
    POSGRADO = any(POSGRADO)) %>%
  # Primera especialidad nacional.
  left_join(
    open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
      left_join(
        open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
          filter(ESPECIALIDAD) %>% 
          group_by(PERSONABASICAID) %>% 
          summarise(
            id_primera_especialidad = min(id_TITULO),
            especialidad_en_periodo_pila = any(between(FECHA_GRADO, ymd('2011-01-01'), ymd('2022-12-31')))
          )
      ) %>% 
      filter(id_primera_especialidad == id_TITULO & !EXTRANJERO) %>% 
      distinct(PERSONABASICAID, especialidad_en_periodo_pila) %>% 
      mutate(
        primera_especialidad_nacional = T
      )
  ) %>% 
  # Médicos que estudiaron en Colombia el pregrado desde 1995 y si tienen
  # especialidad, fue hecha en Colombia.
  filter(
    pregrado_nacional & (
      (ESPECIALIDAD & primera_especialidad_nacional) | !ESPECIALIDAD)
  ) %>%
  rename(personabasicaid = PERSONABASICAID) %>% mutate(ReTHUS = T)

ids_rethus <- df_rethus %>% filter(ESPECIALIDAD) %>% 
  collect %>% select(personabasicaid) %>% unlist %>% unname

df_aux <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>%
  filter(personabasicaid %in% ids_rethus, tipo_cotiz == 21) %>% 
  select(-tipo_cotiz) %>% distinct

df_pila <- df_aux %>% 
  group_by(personabasicaid) %>% 
  summarise(date_start_especializacion = min(fecha_cobertura)) %>% ungroup %>% 
    filter(
      between(date_start_especializacion, ymd('2011-01-01'), ymd('2017-12-31'))
    )

ids_pila <- df_pila %>% collect %>% select(personabasicaid) %>% unlist %>% unname

get_date_dist <- function(df, var1, var2, unit) {
  get_unit_multiplier <- function(unit) {
    possible_unit_multipliers <- c(sec = 1, min = 60, hour = 60, day = 24,
                                   month = 30, trimester = 3, semester = 2, year = 2)
    if (!unit %in% names(possible_unit_multipliers)) {
      stop('Invalid unit.')
    }
    unit_multiplier <- 1
    for (i in 1:length(possible_unit_multipliers)) {
      name <- names(possible_unit_multipliers[i])
      value <- possible_unit_multipliers[[i]]
      unit_multiplier <- unit_multiplier * value
      if (name == unit) {
        break
      }
    }
    return(1 / unit_multiplier)
  }
  
  unit_multiplier <- get_unit_multiplier(unit)
  new_var_name <- sprintf('dist_%s', unit)
  df %>% 
    mutate(!!new_var_name := !!sym(var1) - !!sym(var2)) %>% collect %>% 
    mutate(!!new_var_name := as.numeric(!!sym(new_var_name)) * unit_multiplier) %>% 
    write_parquet('temp.parquet')
  
  df <- open_dataset('temp.parquet')
  return(df)
  
}

# Treatment sample.
df_pila %>% 
  left_join(
    open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
      filter(PREGRADO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>% 
      rename(personabasicaid = PERSONABASICAID,
             fecha_grado_pregrado = FECHA_GRADO)
  ) %>% 
  left_join(
    df_pila %>% 
      left_join(df_aux) %>% 
      get_date_dist('fecha_cobertura', 'date_start_especializacion', 'year') %>% 
      filter(dist_year <= 1.1) %>%
      group_by(personabasicaid) %>% summarise(n_cotizaciones_year1 = n())
  ) %>% 
  mutate(
    # 2011-2017
    treated_1a = T,
    treated_1b = treated_1a & n_cotizaciones_year1 > 3,
    treated_1c = treated_1a & fecha_grado_pregrado < date_start_especializacion,
    treated_1d = treated_1b & fecha_grado_pregrado < date_start_especializacion,
    # 2013-2017
    treated_2a = between(date_start_especializacion, ymd('2013-01-01'), ymd('2017-12-31')),
    treated_2b = treated_2a & n_cotizaciones_year1 > 3,
    treated_3c = treated_2a & fecha_grado_pregrado < date_start_especializacion,
    treated_3d = treated_2b & fecha_grado_pregrado < date_start_especializacion,
    ) %>% 
  # Untreated.
  left_join(
    x = df_rethus %>% 
      select(personabasicaid, ESPECIALIDAD, POSGRADO) %>% 
      left_join(
        open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
          filter(PREGRADO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>% 
          rename(personabasicaid = PERSONABASICAID,
                 fecha_grado_pregrado = FECHA_GRADO)
      )
    ) %>% 
  mutate(control = is.na(date_start_especializacion) & !ESPECIALIDAD & !POSGRADO) %>% 
  filter(treated_1a | control) %>% 
  write_parquet('../New Methodology/PhysiciansPostgraduates/data/treated_samples.parquet')

library(arrow)
library(dplyr)
open_dataset('../Temp JuanRengifo/New Methodology/PhysiciansPostgraduates/data/treated_samples.parquet') %>% glimpse
  summarise(across(matches('control|treated'), ~ sum(., na.rm = T))) %>% collect %>% View

# Descriptive statistics --------------------------------------------------
df_rethus %>% group_by(especialidad_en_periodo_pila) %>% summarise(n()) %>% collect

## {begin} No están en PILA
# Saltos raros en 2013 y 2015.
ids_missing <- setdiff(ids_pila, ids_rethus)
df_pila %>% 
  mutate(
    year = year(date_start_especializacion),
    missing_in_rethus = personabasicaid %in% ids_missing
  ) %>% 
  group_by(year, missing_in_rethus) %>% 
  summarise(n_ = n()) %>% collect %>% 
  group_by(year) %>% 
  mutate(prop_ = n_ / sum(n_))
## {end} No están en ReTHUS.
# {begin} Titulos que tienen los de Rethus (algunos son maestrias).
folder <- 'Z:/Christian Posso/_banrep_research/datos_originales/'
rethus_file <- list.files(folder, recursive = T, full.names = T,
                          pattern = 'RETHUS_reduced') %>% tail(1L)
open_dataset(rethus_file) %>% 
  select(
    personabasicaid = PersonaPersonaBasicaIDPersonaB,
    FECHA_GRADO = ReTHUSFechadeGradoFechaFecha,
    COD_TITULO = ReTHUSPerfilCódigoCódigo,
    TITULO = ReTHUSPerfilPerfilPerfil,
    ORIGEN_TITULO = ReTHUSOrigenObtenciónTítuloOri
  ) %>% 
  filter(personabasicaid %in% ids_missing, COD_TITULO != 'P07') %>% 
  left_join(df_pila) %>% 
  # distinct(PERSONABASICAID) %>% 
  collect %>% View
# {end} Titulos que tienen los de Rethus (algunos son maestrias).

# old ---------------------------------------------------------------------




df_aux <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  ## filter with ReTHUS.
  left_join(x = df_rethus %>% select(personabasicaid)) %>% 
  ##
  filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% distinct
df_pila <- df_aux %>% 
  # ## Correcci[on]
  # distinct %>%
  # left_join(
  #   open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>%
  #     filter(PREGRADO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>%
  #     rename(personabasicaid = PERSONABASICAID,
  #            fecha_grado_pregrado = FECHA_GRADO)
  # ) %>%
  # filter(!is.na(fecha_cobertura),
  #        fecha_grado_pregrado < fecha_cobertura | is.na(fecha_grado_pregrado)) %>%
  ##
group_by(personabasicaid) %>% 
  summarise(date_start_especializacion = min(fecha_cobertura)) %>% ungroup %>% 
  filter(
    between(date_start_especializacion, ymd('2011-01-01'), ymd('2017-12-31'))
  )




df_pila <- df_pila %>% 
  left_join(
    open_dataset('temp.parquet') %>% 
      filter(between(dist_start, 0, 5)) %>%
      group_by(personabasicaid) %>% summarise(n_cotizaciones_5years = n())
  )

df_pila %>% 
  left_join(
    open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
      rename(personabasicaid = PERSONABASICAID)
  ) %>% 
  mutate(dist = FECHA_GRADO - date_start_especializacion) %>% 
  arrange(personabasicaid, dist) %>% collect %>% View
pp %>% filter(n_ == 4) %>% collect
pp %>% collect
pp_id <- 21792345
open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(PERSONABASICAID == pp_id) %>%
  collect
df_aux %>% 
  filter(personabasicaid == pp_id) %>% collect
df_pila %>%   
  mutate(year = year(date_start_especializacion)) %>% 
  group_by(year) %>% summarise(n_ = n()) %>% arrange(year) %>% 
  collect

df_aux <- df_pila %>% mutate(PILA = T) %>% 
  full_join(
    df_rethus %>% filter(especialidad_en_periodo_pila), by = 'personabasicaid'
  ) %>% 
  mutate(PILA = !is.na(PILA), ReTHUS = !is.na(ReTHUS))

df_aux %>% 
  summarise(
    n_only_ReTHUS = sum(ReTHUS & !PILA), 
    n_only_PILA = sum(PILA & !ReTHUS), 
    n_both = sum(PILA & ReTHUS),
    n_total = n()
  ) %>% collect

df_aux %>% 
  mutate(year = year(date_start_especializacion),
         only_PILA = PILA & !ReTHUS) %>% 
  group_by(year, only_PILA) %>% summarise(n_ = n()) %>% 
  arrange(year) %>% 
  collect %>% 
  group_by(year) %>% 
  mutate(total = sum(n_)) %>% 
  filter(only_PILA) %>% 
  mutate(prop = n_ / total) %>% View


df_rethus %>% 
  group_by(
    pregrado_nacional, ESPECIALIDAD, especialidad_en_periodo_pila,
    primera_especialidad_nacional) %>% 
  summarise(n_ = n()) %>% collect
  
