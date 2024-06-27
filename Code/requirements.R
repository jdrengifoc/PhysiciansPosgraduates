requirements <- function(required_libs) {
  isnt_installed <- sapply(required_libs, 
                           function(lib) system.file(package = lib) == "")
  lapply(required_libs[isnt_installed],
         install.packages, character.only = TRUE);
  lapply(required_libs, library, character.only = TRUE);
  cat('\014')
}

required_libs <- c('haven', 'arrow', 'writexl', 'readxl', 'openxlsx',
                   'dplyr', 'lubridate', 'ggplot2', 'stringr', 'tidyr', 'purrr',
                   #'sf', 'ggnewscale',
                   'modelsummary', 'fixest',
                   'beepr', 'tictoc', 'ggthemes',
                   'stringi', 'forcats', 'gsubfn')
requirements(required_libs)
rm(requirements, required_libs)