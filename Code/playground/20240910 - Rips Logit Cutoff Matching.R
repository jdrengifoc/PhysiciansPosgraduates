
#         Callaway & Sant'anna and Matching Rips             #


#     Section 0 - Preliminaries ----

library("foreign"); library("DRDID"); library("haven"); library("staggered")
library("dplyr"); library("ggplot2"); library("purrr"); library("tidyr"); library("did")
library("zoo"); library("MatchIt"); library("caTools"); library("texreg")
library("MASS")

options(scipen = 999) # Force R not to use exponential notation (e.g. e+10)
rm(list=ls(all=TRUE)) # clear the environment

setwd("/Users/nicolasmancera/Dropbox/UnidadVictimas")

set.seed(123)


logit 	    treated edad2010  
predict     pscore , p
save 
# Section 1 - Functions ----

# With replacement 

matching_pscore_1vs1 <- function(data_set) {
  match_result <- matchit(treated ~ pscore, 
                          data = data_set, 
                          method = "nearest",
                          distance = "mahalanobis",
                          replace = TRUE,
                          ratio = 1)
  
  return(get_matches(match_result))
}


# Without replacement

matching_pscore_1vs1_w <- function(data_set) {
  match_result <- matchit(treated ~ pscore, 
                          data = data_set, 
                          method = "nearest",
                          distance = "mahalanobis",
                          replace = FALSE,
                          ratio = 1)

  return(match.data(match_result))
}

# Section 2 - Data ----

# p-score data
data_pscore <- read_dta("Data/Rips/tmp_pscore_matching_data_rips_20240910.dta") 
data_pscore <- subset(data_pscore, !is.na(pscore))

# Section 3 - Matching ----

# 1 vs 1 
model_1vs1_w <- matching_pscore_1vs1_w(data_pscore)

# Section 5 - Export Data ----

# 1 vs 1

model_temp_1vs1_w <- dplyr::select(model_1vs1_w, id_ind_base6b, pscore,treated, subclass)
write_dta(model_temp_1vs1_w, "Data/rips/matching_pscore_1vs1_w_20240910_data_pscore_rr_rf_ind.dta")
