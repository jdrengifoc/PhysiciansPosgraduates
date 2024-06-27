requirements <- function(required_libs) {
  isnt_installed <- sapply(required_libs, 
                           function(lib) system.file(package = lib) == "")
  lapply(required_libs[isnt_installed],
         install.packages, character.only = TRUE);
  lapply(required_libs, library, character.only = TRUE);
  cat('\014')
}

required_libs <- c(
  'haven', 'arrow', 'writexl', 'readxl', 'openxlsx', 'tictoc',
  'dplyr', 'lubridate', 'ggplot2', 'stringr', 'tidyr', 'purrr'
  # 'sf', 'ggnewscale', 'beepr', 'ggthemes', 'stringi', 'forcats', 'gsubfn',
  # 'modelsummary', 'fixest'
  )

requirements(required_libs)
rm(requirements, required_libs)

# GLOBALS
FOLDER_DATOS <- '//wmedesrv/gamma/Christian Posso/_banrep_research/datos_originales'
FOLDER_PROYECTO <- '//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos'