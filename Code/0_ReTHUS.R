source('Code/requirements.R')

folder <- '//wmedesrv/gamma/Christian Posso/_banrep_research/datos_originales/'
rethus_file <- list.files(folder, recursive = T, full.names = T,
                          pattern = 'RETHUS_reduced') %>% tail(1L)

# Rename and select variables in temporary file.
open_dataset(rethus_file) %>% 
  select(
    PERSONABASICAID = PersonaPersonaBasicaIDPersonaB,
    SEXO = PersonaSexoSexo,
    EDAD = PersonaGrupoEtarioEdadesSimple,
    FECHA_ACTO_ADMIN = ReTHUSFechadeActoAdministrativ,
    FECHA_GRADO = ReTHUSFechadeGradoFechaFecha,
    MAYOR_PERFIL = ReTHUSIndicadorPerfilMayorPerfil,
    COD_TITULO = ReTHUSPerfilCódigoCódigo,
    TITULO = ReTHUSPerfilPerfilPerfil,
    MPIO_RESIDENCIA = PersonaGeografiaResidenciaMuni_code,
    DPTO_RESIDENCIA = PersonaGeografiaResidenciaDepa_code,
    TIPO_PROGRAMA = ReTHUSTipodeProgramaTipoProgra,
    ORIGEN_TITULO = ReTHUSOrigenObtenciónTítuloOri,
    tipo_reporte = ReTHUSTipodeReporteTipoReporte
    ) %>% write_parquet('temp.parquet')

# Get physicians graduated since 1995.
ids <- open_dataset('temp.parquet') %>%
  filter(COD_TITULO == 'P07', FECHA_GRADO >= ymd('1995/01/01')) %>%
  distinct(PERSONABASICAID) %>% collect %>% unlist %>% unname
df_aux <- open_dataset('temp.parquet') %>% 
  filter(PERSONABASICAID %in% ids)

# Get demographics (constant per physician). We assume that are current values.
df_aux %>% 
  distinct(PERSONABASICAID, SEXO, EDAD, DPTO_RESIDENCIA, MPIO_RESIDENCIA) %>% 
  write_parquet('../../PhysiciansPosgraduates/Data/ReTHUS_demographics.parquet')

# Get studies. 
# 1. We determine the dates as the minimum date per degree.
# 2. To determine that a degree is national, must have only national values.
# 3. `POSGRADO` exclude specializations.
df_aux %>% 
  group_by(PERSONABASICAID, TITULO) %>% 
  summarise(
    FECHA_ACTO_ADMIN = min(FECHA_ACTO_ADMIN, na.rm = T), 
    FECHA_GRADO = min(FECHA_GRADO, na.rm = T),
    ORIGEN_TITULO = max(ORIGEN_TITULO, na.rm = T)
    ) %>% 
  arrange(PERSONABASICAID, FECHA_GRADO) %>% 
  left_join(
    df_aux %>% 
      distinct(PERSONABASICAID, TITULO, TIPO_PROGRAMA), 
    by = c("PERSONABASICAID", "TITULO")
    ) %>% 
  mutate(
    PREGRADO = str_detect(TITULO, '^P07'),
    POSGRADO = str_detect(TITULO, '^(D|E01|M|Q)'),
    AUXILIAR = str_detect(TITULO, '^A'),
    TyT = str_detect(TITULO, '^T'),
    EXTRANJERO = ORIGEN_TITULO == 2,
    ESPECIALIDAD = TIPO_PROGRAMA == 6
    ) %>% collect %>% 
  group_by(PERSONABASICAID) %>% 
  mutate(id_TITULO = row_number()) %>% 
  write_parquet('../../PhysiciansPosgraduates/Data/ReTHUS_studies.parquet')

unlink('temp.parquet')
rm(df_aux)