setwd('PhysiciansPosgraduates')
source('Code/requirements.R')
library("MatchIt")
library("ISLR")
library(fastDummies)
library(yardstick)
?matchit

df <-  open_dataset(file.path(FOLDER_PROYECTO, 'demographics.parquet')) %>% 
  left_join(
    open_dataset(file.path(FOLDER_PROYECTO, 'Data', 'treated_samples.parquet')) %>% 
      select(personabasicaid, fecha_grado_pregrado, starts_with('treated_'), control),
    by = 'personabasicaid'
  ) %>%
  mutate(
    cont_fecha_grado_pregrado = year(fecha_grado_pregrado) + 
      0.5 * semester(fecha_grado_pregrado)
    ) %>% collect %>% 
  mutate(
    norm_age = (age - min(age)) / (diff(range(age))),
    norm_fecha_grado_pregrado = (cont_fecha_grado_pregrado - min(cont_fecha_grado_pregrado)) / 
      (diff(range(cont_fecha_grado_pregrado))),
  )

# agrupoar los primeros anos
treatment_types <- names(df)[str_detect(names(df), 'treated')]
for (treatment_type in treatment_types) {
  df_treatement <- df %>% 
    rename(treated = !!sym(treatment_type)) %>% 
    filter(treated | is.na(treated)) %>% 
    mutate(treated = !is.na(treated))
  # Get range of fecha_grado
  min_treated_fecha_grado_pregrado <- df_treatement %>% 
    filter(treated) %>% 
    distinct(cont_fecha_grado_pregrado, treated) %>% 
    summarise(min(cont_fecha_grado_pregrado)) %>% pull %>% print
  max_treated_fecha_grado_pregrado <- df_treatement %>% 
    filter(treated) %>% 
    distinct(cont_fecha_grado_pregrado, treated) %>% 
    summarise(max(cont_fecha_grado_pregrado)) %>% pull %>% print
  df_treatement <- df_treatement %>% 
    filter(
      between(cont_fecha_grado_pregrado, 
              min_treated_fecha_grado_pregrado,
              max_treated_fecha_grado_pregrado)
      )
}


df_logit <- df_treatement %>% 
  dummy_cols(select_columns = c('cont_fecha_grado_pregrado', 'age'), 
             remove_first_dummy = T) %>% 
  select(-personabasicaid, -age, - fecha_grado_pregrado, -control, 
         -cont_fecha_grado_pregrado, -norm_age, -norm_fecha_grado_pregrado, 
         -all_of(treatment_types[treatment_types != treatment_type])) 
df_treatement %>% 
  group_by(cont_fecha_grado_pregrado, treated) %>% 
  summarise(n_ = n()) %>% 
  ggplot(aes(x = cont_fecha_grado_pregrado, y = n_, fill = treated)) +
  geom_bar(stat = 'identity')


logit <- glm(treated ~ ., family = binomial(logit), data = df_logit)
logit %>% summary

df_treatement %>% 
  mutate(
    !!sym(paste0('fitted_', treatment_type)) := logit$fitted.values,
    forecast = logit$fitted.values >= 0.4#median(logit$fitted.values)
  ) %>% 
  mutate(
    treated = as.factor(treated),
    forecast = as.factor(forecast)
    ) %>% 
  conf_mat(truth = treated, estimate = forecast) %>% 
  autoplot(type = "heatmap")


df_treatement %>% 
  select(personabasicaid, treated, !!sym(paste0('fitted_', treatment_type))) %>% View
# df_logit_juan <- df_treatement %>% 
#   dummy_cols(select_columns = c('cont_fecha_grado_pregrado'), 
#              remove_first_dummy = T) %>% 
#   select(treated, woman, norm_age, starts_with('cont_fecha_grado_pregrado_')) 
# logit_juan <- glm(treated ~ ., 
#                   family = binomial(logit), data = df_logit_juan)
# logit_juan %>% summary
# 
# df_logit_juan <- df_treatement %>% 
#   dummy_cols(select_columns = c('age'), 
#              remove_first_dummy = T) %>% 
#   select(treated, woman, norm_fecha_grado_pregrado, starts_with('age_')) 
# logit_juan <- glm(treated ~ ., family = binomial(logit), data = df_logit_juan)
# logit_juan %>% summary
