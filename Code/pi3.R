source('Code/requirements.R')

# banrep ------------------------------------------------------------------


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



# home --------------------------------------------------------------------
folder <- 'Output/pi3'
files <- list.files(folder, full.names = T)

library(arrow)
library(dplyr)
library(stringr)

# Directorio de los archivos
folder <- 'Output/pi3'
open_dataset(files[1]) %>% 
  mutate(across(matches('^(treate|ESP|woman)'), ~as.logical(.))) %>% collect()
# Lista de archivos .parquet
files <- list.files(folder, pattern = "\\.parquet$", full.names = TRUE)

# Función para cargar cada archivo, agregar columnas y unificar
df <- files %>%
  lapply(function(file) {
    # Cargar el dataset
    data <- open_dataset(file) %>% 
      mutate(across(matches('^(treate|ESP|woman)'), ~as.logical(.))) %>% collect()
    
    # Detectar si el archivo es "matching" y extraer el código
    is_matching <- str_detect(basename(file), "matching")
    id <- str_extract(basename(file),  "(?<=treated_)[^(\\.|_)]+")
    
    # Renombrar la columna treated_<code> a "treated"
    col_name <- paste0("treated_", id)
    if (col_name %in% colnames(data)) {
      data <- data %>%
        rename(treated = all_of(col_name))
    }
    
    # Agregar las columnas de "matching" y "id"
    data <- data %>%
      mutate(matching = is_matching, id = id)
    
    return(data)
  }) %>%
  bind_rows()


df %>% group_by(id, matching) %>% 
  summarise(n_ = sum(n_)) %>% 
  arrange(id, matching) %>% 
  left_join(
    df %>% group_by(id, matching, treated) %>% 
      summarise(n_ = sum(n_)) %>% 
      arrange(id, matching) %>% 
      filter(!is.na(treated)) %>% 
      spread(treated, n_) %>% 
      rename(treateds = `TRUE`, controls = `FALSE`),
    by = c('id', 'matching')
  ) %>% 
  mutate(pp = n_ == controls + treateds)



# FIGURE 1. barplot_counts_per_id -----------------------------------------

df_plot <- df %>% group_by(id, matching, treated) %>% 
  summarise(n_ = sum(n_)) %>% 
  arrange(id, matching) %>% 
  filter(!is.na(treated)) %>% ungroup()
df_plot %>% 
  filter(matching) %>% spread(treated, n_) %>% 
  rename(controls = `FALSE`, treateds = `TRUE`) %>% 
  mutate(ave_matches = controls / treateds) %>% 
  select(id, ave_matches) %>% write_xlsx('Output/Tables/pi3_ave_matches.xlsx')
ggplot(df_plot, aes(x = id, y = n_/1e3, fill = treated)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  facet_wrap(
    ~matching, scales = 'free_y',
    labeller = labeller(matching = c(`TRUE` = "Matched", `FALSE` = "Unmatched"))) +
  scale_fill_manual(values = c("FALSE" = "#1f78b4", "TRUE" = "#DC3200"),
                    labels = c("FALSE" = "Control", "TRUE" = "Treated")) +
  labs(x = "Group ID", y = "Number of Clusters (Thousands)", fill = "Group",
       title = NULL) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

ggsave("Output/Figures/pi3/barplot_counts_per_id.png", width = 6, height = 4, dpi = 300)



# FIGURE 2. Distribution of covariates --------------------------------------


df_plot <- df %>% ungroup() %>% 
  mutate(aux = year(fechapregrado) + semester(fechapregrado)/2) %>% 
  mutate(semestre_grado = coalesce(semestre_grado, aux)) %>% 
  select(id, matching, treated, semestre_grado) %>% 
  filter(!is.na(treated))


df_plot %>% filter(treated, matching) %>% mutate(matching = !matching) %>% 
  bind_rows(df_plot %>% filter(!(treated & !matching))) %>% 
  ggplot(aes(x = matching, y = semestre_grado, fill = treated, )) +
  geom_violin(trim = FALSE) + # Adjusted to avoid trimming the tails of the violin plot
  coord_flip() +
  facet_wrap(~id, scales = "free_y", nrow = 2) + # Scales adjusted for clarity
  scale_fill_manual(values = c("FALSE" = "#1f78b4", "TRUE" = "#DC3200"),
                    labels = c("FALSE" = "Control", "TRUE" = "Treated")) +
  labs(x = "Matching", y = "Graduation semester of medicine", fill = "Group") +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5),  # Ensuring x-axis labels are clear
    panel.spacing = unit(1, "lines")  # Adjust spacing between facets
  )


ggsave("Output/Figures/pi3/balance_comparison_graduation.png", width = 10, height = 5, dpi = 300)

list.files("Output/Figures/pi3")


# ES plots ----------------------------------------------------------------



x_min_lim <- -7
x_max_lim <- 5
# Define filter variables
filter_variables <- c('cohort', 'controls', 'wboot', 'estimation', 
                      'outcome', 'treatment_group')

es_results <- bind_rows(
  read_dta('Output/Tables/CS_results_matching.dta') %>%
    filter(!str_detect(var, '_avg')) %>%
    mutate(
      wboot = as.logical(wboot), 
      controls = as.logical(controls),
      dist = str_replace(var, 'tm', '-') %>% str_replace('tp', '') %>% as.integer()
    ) %>%
    filter(between(dist, x_min_lim, x_max_lim)) %>% 
    arrange(across(all_of(filter_variables))) %>% 
    mutate(matching = T),
  read_dta('Output/Tables/CS_results.dta') %>%
    filter(!str_detect(var, '_avg')) %>%
    mutate(
      wboot = as.logical(wboot), 
      controls = as.logical(controls),
      dist = str_replace(var, 'tm', '-') %>% str_replace('tp', '') %>% as.integer()
    ) %>%
    filter(between(dist, x_min_lim, x_max_lim)) %>% 
    arrange(across(all_of(filter_variables))) %>% 
    mutate(matching = F),
)


# FIGURE 3. wage ----------------------------------------------------------


df_plot <- es_results %>% 
  filter(wboot, controls, cohort == 1995, !matching, 
         estimation == 'ET', outcome == 'pila_salario_r') %>% 
  mutate(treatment_group = gsub("treated_", "", treatment_group))

ggplot(df_plot, 
       aes(
         x = as.factor(dist), y = coef / 1e6,
         color = treatment_group
       )) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.25) +  # Horizontal line at y = 0
  geom_point() +  # Adjusted point size for better visibility
  geom_errorbar(aes(ymin = ci_lower / 1e6, ymax = ci_upper / 1e6), width = 0.3) +  # Error bars
  labs(x = "Semesters from Enrollment", y = "Estimates (Millions)",
       color = "Group ID") +  # Axis labels
  scale_y_continuous(n.breaks = 10) +  # Control y-axis breaks for readability
  scale_color_manual(values = c(
    "1a" = "#1f78b4", 
    "1b" = "#33a02c", 
    "1c" = "#e31a1c", 
    "1d" = "#ff7f00",
    "2a" = "#6a3d9a", 
    "2b" = "#b15928", 
    "2c" = "#a6cee3", 
    "2d" = "#b2df8a"
  )) +  # Assign custom colors to treatment groups
  theme_minimal(base_size = 14) +  # Larger base font size for readability
  theme(
    legend.position = "top",  # Place the legend at the bottom
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),  # Make legend text readable
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Larger, bold axis titles
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.ticks = element_line(color = "gray50")  # Subtle ticks
  )

ggsave("Output/Figures/pi3/es_plot_wage_NT.png", width = 5, height = 4, dpi = 300)

# FIGURE 4. mental heatl ----------------------------------------------------------

df_plot <- es_results %>% 
  filter(wboot, controls, cohort == 1995, !matching,
         estimation == 'NET', outcome == 'service_mental') %>% 
  mutate(treatment_group = gsub("treated_", "", treatment_group))


ggplot(df_plot, 
       aes(
         x = as.factor(dist), y = coef,
         color = treatment_group
       )) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.25) +  # Horizontal line at y = 0
  geom_point() +  # Adjusted point size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5) +  # Error bars
  labs(x = "Semesters from Enrollment", y = "Estimates", 
       color = 'Group ID') +  # Axis labels
  scale_y_continuous(n.breaks = 10) +  # Control y-axis breaks for readability
  scale_color_manual(values = c(
    "1a" = "#1f78b4", 
    "1b" = "#33a02c", 
    "1c" = "#e31a1c", 
    "1d" = "#ff7f00",
    "2a" = "#6a3d9a", 
    "2b" = "#b15928", 
    "2c" = "#a6cee3", 
    "2d" = "#b2df8a"
  )) +  # Assign custom colors to treatment groups
  theme_minimal(base_size = 14) +  # Larger base font size for readability
  theme(
    legend.position = "top",  # Place the legend at the bottom
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),  # Make legend text readable
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Larger, bold axis titles
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.ticks = element_line(color = "gray50")  # Subtle ticks
  )

ggsave("Output/Figures/pi3/es_plot_mental_NYT.png", width = 5, height = 4, dpi = 300)




# FIGURE 5. wages + matching ----------------------------------------------



df_plot <- es_results %>% 
  mutate(treatment_group = gsub("treated_", "", treatment_group)) %>% 
  filter(wboot, controls, cohort == 1995,# treatment_group == '1a',
         estimation == 'ET', outcome == 'pila_salario_r')

ggplot(df_plot, 
       aes(
         x = as.factor(dist), y = coef / 1e6,
         color = matching
       )) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.25) +  # Horizontal line at y = 0
  geom_point() +  # Adjusted point size for better visibility
  geom_errorbar(aes(ymin = ci_lower / 1e6, ymax = ci_upper / 1e6), width = 0.3) +  # Error bars
  labs(x = "Semesters from Enrollment", 
       y = "Estimates (Millions)", color = "Matching") +  # Axis labels
  scale_y_continuous(n.breaks = 10) +  # Control y-axis breaks for readability
  scale_color_manual(values = c(
    "TRUE" = "#1f78b4", 
    "FALSE" = "#e31a1c"
  )) +  # Assign custom colors to treatment groups
  theme_minimal(base_size = 14) +  # Larger base font size for readability
  theme(
    legend.position = "top",  # Place the legend at the bottom
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),  # Make legend text readable
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Larger, bold axis titles
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.ticks = element_line(color = "gray50")  # Subtle ticks
  )

ggsave("Output/Figures/pi3/es_plot_wage_NT_matching.png", width = 5, height = 4, dpi = 300)



# FIGURE 6. mental health + matching --------------------------------------

df_plot <- es_results %>% 
  filter(wboot, controls, cohort == 1995,
         estimation == 'NET', outcome == 'service_mental') %>% 
  mutate(treatment_group = gsub("treated_", "", treatment_group))


ggplot(df_plot, 
       aes(
         x = as.factor(dist), y = coef,
         color = matching
       )) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.25) +  # Horizontal line at y = 0
  geom_point() +  # Adjusted point size for better visibility
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5) +  # Error bars
  labs(x = "Semesters from Enrollment", y = "Estimates", 
       color = 'Group ID') +  # Axis labels
  scale_y_continuous(n.breaks = 10) +  # Control y-axis breaks for readability
  scale_color_manual(values = c(
    "TRUE" = "#1f78b4", 
    "FALSE" = "#e31a1c"
  )) +  # Assign custom colors to treatment groups
  theme_minimal(base_size = 14) +  # Larger base font size for readability
  theme(
    legend.position = "top",  # Place the legend at the bottom
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),  # Make legend text readable
    axis.text = element_text(size = 12),  # Increase axis text size
    axis.title = element_text(size = 14, face = "bold"),  # Larger, bold axis titles
    panel.grid.major = element_line(color = "gray90"),  # Light major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    axis.ticks = element_line(color = "gray50")  # Subtle ticks
  )

ggsave("Output/Figures/pi3/es_plot_mental_NYT_matching.png", width = 5, height = 4, dpi = 300)

