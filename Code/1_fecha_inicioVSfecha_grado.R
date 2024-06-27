
# Proposal ----------------------------------------------------------------
# 1. EXTRANJEROS
# 2. PREGADO
# 2. Primera especialidad
# 3. Una sola especialidad
# 3. SOLO rethus
# # Cuantos tratados y cuantos controles. distancia fecha.
# 
# Grafica de deserción
# # Explicacion Subida de los salarios en el grupo de control
# 1. Solo ReTHUS.
# 2. Comenzaron muy rapido el posgrado entonces no trabajaron.


# Especialistas graduados (ReTHUS) en periodo PILA.
df_grados <- open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(ESPECIALIDAD, 
         ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
  distinct %>% 
  group_by(PERSONABASICAID) %>% 
  summarise(n_especialidad = n()) %>% 
  rename(personabasicaid = PERSONABASICAID)

df_estudio_fixed <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% distinct %>% 
  left_join(
    open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
      filter(PREGRADO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>% 
      rename(personabasicaid = PERSONABASICAID,
             fecha_grado_pregrado = FECHA_GRADO)
  ) %>% 
  filter(!is.na(fecha_cobertura),
         fecha_grado_pregrado < fecha_cobertura | is.na(fecha_grado_pregrado))

df_estudio_fixed %>% distinct(personabasicaid) %>% mutate(PILA = T) %>% 
  full_join(
    df_grados %>% distinct(personabasicaid) %>% mutate(ReTHUS = T),
    by = 'personabasicaid'
  ) %>% 
  mutate(PILA = !is.na(PILA), ReTHUS = !is.na(ReTHUS)) %>% 
  summarise(
    n_only_ReTHUS = sum(ReTHUS & !PILA), 
    n_only_PILA = sum(PILA & !ReTHUS), 
    n_both = sum(PILA & ReTHUS),
    n_total = n()
  ) %>% collect

# PILA Vs ReTHUS (exploration) ----------------------------------------------------------

# Especialistas graduados (ReTHUS) en periodo PILA.
df_grados <- open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(ESPECIALIDAD, !EXTRANJERO, 
         ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
  distinct %>% 
  group_by(PERSONABASICAID) %>% 
  summarise(n_especialidad = n()) %>% 
  rename(personabasicaid = PERSONABASICAID)
# Validation.
open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(PREGRADO, !EXTRANJERO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>% 
  rename(personabasicaid = PERSONABASICAID,
         fecha_grado_pregrado = FECHA_GRADO) %>% 
  filter(is.na(fecha_grado_pregrado)) %>% collect
# Personas cotizando como si estudiaran una especialidad medica (PILA)
df_estudio <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% distinct
ids <- df_estudio %>% distinct(personabasicaid) %>% collect %>% unlist %>% unname

df_estudio_fixed <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% distinct %>% 
  left_join(
    open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
      filter(PREGRADO, !EXTRANJERO) %>% distinct(PERSONABASICAID, FECHA_GRADO) %>% 
      rename(personabasicaid = PERSONABASICAID,
             fecha_grado_pregrado = FECHA_GRADO)
  ) %>% 
  filter(!is.na(fecha_cobertura),
         fecha_grado_pregrado < fecha_cobertura | is.na(fecha_grado_pregrado))

# Cuantos identificamos en cada base?
df_estudio_fixed %>% distinct(personabasicaid) %>% mutate(PILA = T) %>% 
  full_join(
    df_grados %>% distinct(personabasicaid) %>% mutate(ReTHUS = T),
    by = 'personabasicaid'
  ) %>% 
  mutate(PILA = !is.na(PILA), ReTHUS = !is.na(ReTHUS)) %>% 
  summarise(
    n_only_ReTHUS = sum(ReTHUS & !PILA), 
    n_only_PILA = sum(PILA & !ReTHUS), 
    n_both = sum(PILA & ReTHUS),
    n_total = n()
    ) %>% collect

## Hay pregrados con codigo 21?
df_plot <- open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(PREGRADO, !EXTRANJERO,
         ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
  # filter(PERSONABASICAID %in% ids) %>%
  distinct(PERSONABASICAID, FECHA_GRADO) %>% 
  left_join(df_estudio, by = join_by(PERSONABASICAID == personabasicaid)) %>% 
  # Cotizan en ReTHUS antes de terminar el pregrado.
  filter(fecha_cobertura <= FECHA_GRADO) %>% 
  # mutate(year = year(fecha_cobertura)) %>% 
  mutate(year = year(fecha_cobertura) + (semester(fecha_cobertura) - 1)/2) %>%
  distinct(PERSONABASICAID, year) %>% 
  group_by(year) %>% summarise(n_estudiantes = n()) %>% collect

df_plot %>% summarise(sum(n_estudiantes))

df_plot %>% 
  ggplot(aes(x = year, y = n_estudiantes)) + 
  geom_bar(stat = 'identity') +
  theme4policies() +
  scale_x_continuous(breaks = 2008:2022, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 3e2, 25), expand = c(0, 0))

## Grados Vs estudiantes por año
rbind(
  df_estudio %>% 
    # mutate(year = year(fecha_cobertura)) %>% 
    mutate(year = year(fecha_cobertura) + (semester(fecha_cobertura) - 1)/2) %>% 
    distinct(year, personabasicaid) %>% 
    # group_by(personabasicaid) %>%
    # summarise(year = max(year)) %>%
    group_by(year) %>% summarise(n_ = n()) %>%
    mutate(tipo = 'estudiantes') %>% collect,
  open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
    filter(ESPECIALIDAD, !EXTRANJERO, 
           ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
    distinct %>% 
    # mutate(year = year(FECHA_GRADO)) %>%
    mutate(year = year(FECHA_GRADO) + (semester(FECHA_GRADO) - 1)/2) %>%
    group_by(year) %>% summarise(n_ = n()) %>% 
    mutate(tipo = 'grados') %>% collect
) %>% 
  # pivot_wider(names_from = tipo, values_from = n_) %>%
  # ggplot(aes(x = lag(estudiantes, 3), y = grados)) +
  # geom_point()
  filter(year < 2022) %>% 
  ggplot(aes(x = year, y = n_, fill = tipo)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme4policies() +
  scale_x_continuous(breaks = 2008:2022, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1e3, 5e1)*10, expand = c(0, 0))


## Personas sin especialidad en ReTHUS que estudiaron según PILA por año.
df_estudio %>% left_join(df_grados) %>% 
  filter(is.na(n_especialidad)) %>% 
  mutate(year = year(fecha_cobertura) + (semester(fecha_cobertura) - 1)/2) %>% 
  distinct(personabasicaid, year) %>% 
  collect %>% 
  ggplot(aes(x = year)) + 
  geom_bar() +
  theme4policies() +
  scale_x_continuous(breaks = 2008:2022, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 6e3, 5e2), expand = c(0, 0))

## Cotizaciones como estudiantes en PILA después del último registro en ReTHUS por año.
## SOLO USAR EL PRIMER POSGRADO.
open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  # Último grado en ReTHUS.
  filter(ESPECIALIDAD, !EXTRANJERO, 
         ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
  group_by(PERSONABASICAID) %>% 
  summarise(max_FECHA_GRADO = max(FECHA_GRADO, na.rm = T)) %>% 
  left_join(df_estudio, by = join_by(PERSONABASICAID == personabasicaid)) %>% 
  # Cotizan en ReTHUS después del grado.
  filter(max_FECHA_GRADO < fecha_cobertura) %>% collect %>%  
  mutate(year = year(fecha_cobertura) + (semester(fecha_cobertura) - 1)/2) %>% 
  group_by(year) %>% summarise(n_cotizaciones = n()) %>% 
  collect %>% 
  ggplot(aes(x = year, y = n_cotizaciones)) + 
  geom_bar(stat = 'identity') +
  theme4policies() +
  scale_x_continuous(breaks = 2008:2022, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 6e3, 5e2), expand = c(0, 0))

# Create panel
open_dataset('../../PhysiciansPosgraduates/Data/RETHUS_studies.parquet') %>% 
  filter(ESPECIALIDAD, !EXTRANJERO, 
         ymd('2008/02/01') <= FECHA_GRADO, FECHA_GRADO <= ymd('2022/12/31')) %>%
  distinct(PERSONABASICAID, FECHA_GRADO) %>% collect %>% 
  group_by(PERSONABASICAID) %>% 
  mutate(id_TITULO = paste0('fecha_grado_', row_number())) %>%  ungroup %>% 
  pivot_wider(names_from = id_TITULO, values_from = FECHA_GRADO) %>% 
  write_parquet('temp.parquet')

# 2008 2009 2010 2011-2013 2014 2015
# PILA PILA PILA     NA    PILA PILA
#           posgrado1      posgrado2
# up21 up21 up21
# up22 up22 up22           up22 up22
open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  mutate(estudiando = tipo_cotiz == 21, in_PILA = T) %>% 
  select(-tipo_cotiz) %>% distinct %>% 
  full_join(
    open_dataset('temp.parquet') %>%
      mutate(pp = PERSONABASICAID, in_ReTHUS = T),
    by = join_by(personabasicaid==PERSONABASICAID)
    ) %>%
  mutate(
    in_PILA = !is.na(in_PILA), in_ReTHUS = !is.na(in_ReTHUS),
    up2_grado_1 = fecha_cobertura <= fecha_grado_1,
    up2_grado_2 = fecha_cobertura <= fecha_grado_2,
    up2_grado_3 = fecha_cobertura <= fecha_grado_3,
    estudio_grado_1 = fecha_cobertura <= fecha_grado_1 & estudiando,
    estudio_grado_2 = fecha_cobertura <= fecha_grado_2 & estudiando,
    estudio_grado_3 = fecha_cobertura <= fecha_grado_3 & estudiando
    ) %>%
  mutate(
    estudio_grado_3 = !estudio_grado_2 & estudio_grado_3,
    estudio_grado_2 = !estudio_grado_1 & estudio_grado_2,
    up2_grado_3 = !up2_grado_2 & up2_grado_3,
    up2_grado_2 = !up2_grado_1 & up2_grado_2,
    personabasicaid = coalesce(personabasicaid, pp)
    ) %>%
  select(-pp) %>%
  relocate(personabasicaid) %>%
  arrange(personabasicaid, fecha_cobertura) %>% ungroup %>%
  write_parquet('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet')

# Validacion panel
open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet') %>% 
  distinct(personabasicaid, estudiando) %>% 
  filter(estudiando) %>% collect %>% nrow

open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet') %>% 
open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet') %>% 
  distinct(personabasicaid, in_ReTHUS) %>% 
  filter(in_ReTHUS) %>% collect %>% nrow

# Se graduan y siguen cotizando con el mismo codigo.
open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet') %>%
  filter(estudiando, !estudio_grado_1, !estudio_grado_2, !estudio_grado_3) %>% 
  collect %>% View

# Estudian y trabajan? SI EL CODIGO ES ROBUSTO A TRABAJADORES.
df_aux <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% 
  distinct(personabasicaid, fecha_cobertura) %>% 
  left_join(
    open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet')
  ) %>%  
  group_by(personabasicaid, fecha_cobertura) %>%   
  summarise(
    n_cotizaciones = n()
    ) %>% 
  filter(n_cotizaciones > 1) %>% 
  left_join(
    open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet')
  ) %>% 
  group_by(personabasicaid, fecha_cobertura) %>%   
  summarise(trabajan = T)

open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet') %>%
  filter(estudiando) %>% 
  left_join(df_aux) %>% 
  mutate(trabajan = !is.na(trabajan)) %>% 
  summarise(
    n_solo_estudiando = sum(estudiando & !trabajan), 
    n_trabajan = sum(trabajan),
    n_estudiando = sum(estudiando)
    ) %>% 
  collect


# Los que no "estudian" cotizaron antes del grado
# Los que no aparecen en PILA no es que no esten cotizando. Pero se soluciona
# si eliminamos los que solo estan en ReTHUS.
df_grados %>% left_join(
  open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
    filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% 
    distinct(personabasicaid) %>% mutate(merge_ = F)
  ) %>% filter(is.na(merge_)) %>% distinct(personabasicaid) %>% 
  left_join(
    open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet')
    ) %>% 
  group_by(personabasicaid, fecha_grado_1, fecha_grado_2) %>% 
  summarise(
    cotizo_antes_grado_1 = any(up2_grado_1, na.rm = T),
    cotizo_antes_grado_2 = any(up2_grado_2, na.rm = T),
  ) %>% ungroup %>% 
  mutate(
    cotizo_antes_grado_1 = if_else(is.na(fecha_grado_1), NA, cotizo_antes_grado_1),
    cotizo_antes_grado_2 = if_else(is.na(fecha_grado_2), NA, cotizo_antes_grado_2)
  ) %>% 
  summarise(
    n_cotizaron_grado_1 = sum(cotizo_antes_grado_1, na.rm = T),
    n_cotizaron_grado_2 = sum(cotizo_antes_grado_2, na.rm = T)
    ) %>% 
  collect

# Los que no "estudian" cotizaron en los tres años previos al grado
df_grados %>% left_join(
  open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
    filter(tipo_cotiz == 21) %>% select(-tipo_cotiz) %>% 
    distinct(personabasicaid) %>% mutate(merge_ = F)
) %>% filter(is.na(merge_)) %>% distinct(personabasicaid) %>% 
  left_join(
    open_dataset('../New Methodology/PhysiciansPostgraduates/mini_panel.parquet')
  ) %>% 
  group_by(personabasicaid) %>% 
  mutate(
    dist_grado1 = fecha_grado_1 - fecha_cobertura,
    dist_grado2 = fecha_grado_2 - fecha_cobertura
    ) %>% write_parquet('temp.parquet')

open_dataset(
  'temp.parquet',
  schema = schema(
    personabasicaid = double(),
    fecha_cobertura = date32(),
    fecha_grado_1 = date32(),
    fecha_grado_2 = date32(),
    dist_grado1 = int64(),
    dist_grado2 = int64()
    )
  ) %>% 
  distinct %>% 
  mutate(
    dist_grado1 =  as.numeric(dist_grado1) / 3600 / 24, 
    dist_grado2 =  as.numeric(dist_grado2) / 3600 / 24,
    radius_grado1 = between(dist_grado1, 0, 2*365),
    radius_grado2 = between(dist_grado2, 0, 2*365)
    ) %>% collect %>% 
  filter(radius_grado1 | radius_grado2) %>%
  group_by(personabasicaid) %>% 
  summarise(
    n_cotizaciones_grado1 = sum(radius_grado1),
    n_cotizaciones_grado2 = sum(radius_grado2)
  ) %>% ungroup %>% 
  pivot_longer(
    cols = starts_with('n_'),  names_prefix = 'n_cotizaciones_grado',
    names_to = 'grado', values_to = 'n_cotizaciones') %>% 
  mutate(n_cotizaciones = as.integer(n_cotizaciones)) %>% 
  group_by(grado, n_cotizaciones) %>% summarise(n_ = n()) %>% 
  ggplot(aes(x = n_cotizaciones, y = n_, fill = grado)) + 
  facet_wrap(vars(grado), scales = 'free', ncol = 1) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 0:24)
  

  ## cuantas cotizaciones tienen estudiando la especialidad vs trabajando.
df_aux <- open_dataset('../New Methodology/PhysiciansPostgraduates/mini_history_PILA.parquet') %>% 
  filter(personabasicaid %in% ids) %>% 
  mutate(especialidad = tipo_cotiz == 21) %>%
  distinct()

df_plot <- df_aux %>% 
  # Contar cotizaciones estudiando.
  group_by(personabasicaid, especialidad) %>% 
  summarise(n_cotizaciones = n()) %>% 
  # Agregar total cotizaciones.
  left_join(
    df_aux %>% 
      group_by(personabasicaid) %>% 
      summarise(total_cotizaciones = n())
  ) %>% 
  # Agregar número especialidades
  left_join(df_grados, by = 'personabasicaid')

# Personas por número de años estudiando
df_plot %>%   
  filter(especialidad) %>% 
  mutate(years_studying = n_cotizaciones / 12) %>% 
  collect %>% 
  ggplot(aes(x = years_studying)) +
  geom_histogram() +
  theme4policies() +
  scale_x_continuous(breaks = 0:13, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Años estudiando por numero de especialidades.
df_plot %>%   
  filter(especialidad, !is.na(n_especialidad)) %>% 
  mutate(years_studying = n_cotizaciones / 12) %>% 
  collect %>% 
  ggplot(aes(x = n_especialidad, y = years_studying, group = n_especialidad)) +
  geom_violin() +
  theme4policies() +
  scale_y_continuous(breaks = 0:13, expand = c(0, 0))


# Unsolved paths ----------------------------------------------------------

# Proporcion del tiempo estudiando.
df_plot %>%   
  filter(especialidad) %>% 
  mutate(prop_estudio = 100 * n_cotizaciones / total_cotizaciones) %>% 
  collect %>% 
  ggplot(aes(x = prop_estudio, fill = especialidad)) +
  geom_histogram()


df_estudio %>% left_join(df_aux %>% rename(date = DATE)) %>% 
  group_by(personabasicaid) %>% 
  mutate(diff_date = DATE - date) %>% 
  write_parquet('temp.parquet')

# Numero de cotizaciones estudiando por 
df_plot <- open_dataset('temp.parquet', 
             schema = schema(
               personabasicaid = double(),
               DATE = date32(),
               diff_date = int64()
               )
             ) %>% 
  group_by(personabasicaid, DATE) %>% 
  summarise(continue = any(between(abs(diff_date / 3600 / 24), 28, 31))) %>% 
  left_join(df_grados) %>% 
  group_by(personabasicaid, continue, n_especialidad) %>% 
  summarise(n_cotizaciones_seguidas = n())# %>% collect %>% View
  # group_by(continue, n_cotizaciones_seguidas, n_especialidad) %>% summarise(n_ = n())

df_plot %>% 
  mutate(years_cotizaciones_seguidas = n_cotizaciones_seguidas / 12) %>% 
  filter(continue) %>% 
  collect %>% 
  ggplot(aes(x = n_cotizaciones_seguidas)) +
  facet_grid(~n_especialidad, scale = 'free') +
  geom_histogram() +
  theme4policies()


