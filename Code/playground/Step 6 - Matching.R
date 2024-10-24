#------------------------------------------------------------------------------- 
# Creation date: 18/06/2024
# Description: This code performs a matching exercise to compare treated and 
# untreated individuals (mental health therapy within 90 days after 
# diagnosis) based on 2009 covariates. 
#------------------------------------------------------------------------------- 
# Required packages
paquetes <- c("foreign", "haven", "dplyr", "MatchIt", "purrr", "tidyr", "caTools", "zoo", "MASS")
for (paquete in paquetes) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
  }
  library(paquete, character.only = TRUE)
}

options(scipen = 999) # Force R to not use exponential notation (e.g. e+10)
rm(list=ls(all=TRUE)) # clear the environment


# Working directory
hostname <- Sys.info()[["nodename"]]

if (hostname == "SM201439") {
  pc <- "C:"
} else {
  pc <- "\\sm093119"
}

wd <- file.path(pc, "Proyectos", "Banrep research", "Health_shocks_and_labor", "Data")

# setwd(wd)
set.seed(1234)


#------------------------------------------------------------------------------- 
#                           Matching functions
#------------------------------------------------------------------------------- 
# With replacement 
match_rep <- function(data_set) {
  match_result <- matchit(treatment ~ pscore, data = data_set, method = "nearest",
                          distance = "mahalanobis", replace = TRUE, ratio = 1)
  
  return(get_matches(match_result))
}


# Without replacement
match_norep <- function(data_set) {
  match_result <- matchit(treatment ~ pscore, data = data_set, method = "nearest",
                          distance = "mahalanobis", replace = FALSE, ratio = 1)
  
  return(match.data(match_result))
}


#------------------------------------------------------------------------------- 
#                                  Matching
#-------------------------------------------------------------------------------
data <- read_dta("Data/Depression_young_pscore.dta")
data <- subset(data, !is.na(pscore))

d_match_rep <- match_rep(data)
d_match_norep <- match_norep(data)


# Export data
model_rep <- dplyr::select(d_match_rep, id_event, subclass, pscore)
write_dta(model_rep, "Data/Depression_matching_replace.dta")

model_norep <- dplyr::select(d_match_norep, id_event, subclass, pscore)
write_dta(model_norep, "Data/Depression_matching_Noreplace.dta")
