setwd('PhysiciansPosgraduates')
source('Code/requirements.R')
library(MatchIt)
library(ISLR)
library(fastDummies)
library(yardstick)


# Preprocessing -----------------------------------------------------------

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

# Process by treatment group ----------------------------------------------
# agrupoar los primeros anos?
treatment_types <- names(df)[str_detect(names(df), 'treated')]
treatment_type <- treatment_types[1]
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


# Plots -------------------------------------------------------------------

# Número de tratados y controles por semestre.
df_treatement %>% 
  group_by(cont_fecha_grado_pregrado, treated) %>% 
  summarise(n_ = n()) %>% 
  ggplot(aes(x = cont_fecha_grado_pregrado, y = n_, fill = treated)) +
  geom_bar(stat = 'identity')

#' Cuando adicionamos una más y otra entonces la dimensionalidad crece mucho
#' Entonces lo primero es pensar que ese es el problema 
#' Cuando adicionamos una más y otra entonces la dimensionalidad crece mucho
#' Entonces lo primero es pensar que ese es el problema 
#' 
#' ¿Qué queremos nosotros ? 
#' Un extremo es que por cada tratado, encontremos un control igual en esas tres dimensiones
#' Entonces creemos celdas que combinen por cada covariable y contemos cuántas 
#' observaciones tenemos tanto en controles como tratados 
#' 
#' Hagamos una gráfica que se superpongan todas esas celdas con colores 
#' diferentes para controles y tratados
#' (color oscuro cuando el tratado tiene menos de 10 obs (o 5 )) en una celda 
#' 
#' Puedes quitar de este análisis todos los controles que estén por fuera de los
#' semestres de graduación que consideramos y los rangos de edad


# EJEMPLO CASA.
graduation_years <- 1995:2017
semesters <- paste(rep(graduation_years, each = 2), 
                   rep(1:2, length(graduation_years)), 
                   sep = '-')
n <- 9e4
df <- data.frame(
  id = 1:n,
  mujer = sample(c(T, F), n, replace = T),
  edad = sample(20:72, n, replace = T),
  semestre_grado = sample(semesters, n, replace = T),
  treated = runif(n) <= 0.01
)

df %>% 
  group_by(treated, mujer, edad, semestre_grado) %>% summarise(n_ = n()) %>% 
  mutate(bin = paste(c('h', 'm')[mujer + 1], edad, semestre_grado, sep = '/')) %>% 
  arrange(bin) %>% View
  ggplot(aes(x = bin, y = n_ * (n_ < 10), fill = treated)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~treated, scale = 'free_y', nrow = 2) + 
  coord_flip()
df %>% filter(treated) %>% rename(id_treated = id) %>% select(-treated) %>% 
  left_join(
    df %>% filter(!treated) %>% rename(id_control = id) %>% select(-treated),
    by = c('mujer', 'edad', 'semestre_grado')
  ) %>% View
df %>% filter(treated) %>% nrow
m1 <- matchit(formula = treated ~ mujer + edad + semestre_grado, data = df,
              method = 'exact')
m2 <- matchit(formula = treated ~ mujer + edad + semestre_grado, data = df,
              method = 'nearest', distance = "glm")
match.data(m1) %>% 
  filter(subclass == 3)
match.data(m2) %>% head
  filter(id == 849)
  arrange(desc(subclass)) %>% 
  group_by(subclass) %>% 
  summarise(n_ = n())
m1$weights %>% View
m1 %>% summary()

# En el banco
df_covariates <- df_treatement %>% 
  select(personabasicaid, woman, age, treated, 
         semester_graduation = cont_fecha_grado_pregrado)

df_count <- df_covariates %>% group_by(woman, age, treated, semester_graduation) %>% 
  summarise(n_ = n()) %>% ungroup()

message("total obs: ", df_count$n_ %>% sum)
message("total treated obs: ", df_count %>% filter(treated) %>% pull(n_) %>% sum)

df_matching <- df_count %>% 
  filter(treated) %>% rename(n_treated = n_) %>% select(-treated) %>% 
  left_join(
    df_count %>% 
      filter(!treated) %>% rename(n_control = n_) %>% select(-treated),
    by = c('woman', 'age', 'semester_graduation')
  ) %>% 
  mutate(
    n_control = if_else(is.na(n_control), 0, n_control),
    controls_per_treated = n_control / n_treated
    )#filter(!is.na(n_control))


df_matching %>% pull(n_treated) %>% sum
df_matching %>% pull(n_control) %>% sum

df_matching %>% 
  ggplot(aes(x = n_control)) +
  geom_density(color = 'red')

df_matching %>% group_by(n_control) %>% summarise(n_ = n()) %>% ungroup() %>% 
  ggplot(aes(x = n_control, y = n_)) +
  geom_bar(stat = 'identity')

df_matching %>%  
  ggplot(aes(x = controls_per_treated)) +
  geom_histogram(binwidth = 5)

df_matching %>% pull(controls_per_treated) %>% mean
df_matching %>% pull(controls_per_treated) %>% median()
prct <- seq(0, 1, 0.05)
tream <- lapply(
  prct, 
  function(x) mean(df_matching %>% pull(controls_per_treated), trim = x)) %>% 
  unlist
plot(prct, tream)


df %>% filter(treated) %>% 
  rename(id_treated = personabasicaid) %>% select(-treated) %>% 
  distinct(woman, age, semester_graduation, .keep_all = T) %>% 
  distinct(id_treated) %>% nrow
# Logit -------------------------------------------------------------------

df_logit <- df_treatement %>% 
  dummy_cols(select_columns = c('cont_fecha_grado_pregrado', 'age'), 
             remove_first_dummy = T) %>% 
  select(-personabasicaid, -age, - fecha_grado_pregrado, -control, 
         -cont_fecha_grado_pregrado, -norm_age, -norm_fecha_grado_pregrado, 
         -all_of(treatment_types[treatment_types != treatment_type])) 



# Logit all dummies.
logit <- glm(treated ~ ., family = binomial(logit), data = df_logit)
logit %>% summary

# Confusion matrix
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



# Other logits ------------------------------------------------------------
# Peor AIC
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


# matching ----------------------------------------------------------------

