library(here)
library(magrittr)
library(purrr)
library(tidyverse)

library(ggplot2)

# Clear workspace
rm(list = ls())

# --- Input parameters ---

# files to process
par.files <- c("data_exp1_psycurve.csv",
               "data_exp2_psycurve.csv",
               "data_exp1_staircases.csv",
               "data_exp2_staircases.csv")

# which columns define groups, depending on the dataset (see below)
par.columns <- c("Subject", "Distance", "Condition", "Start")

# process one single dataset at a time
for (aux.filename in par.files) {
  
  # get full path
  aux.fullpath <- here("data", aux.filename)

  # load data
  cat("Loading \"", aux.filename, "\"... ", sep="")
  data <- aux.fullpath %>%
    read_csv(show_col_types = FALSE,     # \_ silent output
             progress = FALSE,           # /
             lazy     = FALSE) %>%       # required in Windows to prevent locking the file
    mutate(logRT = log10(RT))
  cat("Processing... ")

  # dynamically select the group columns for each dataset
  aux.cols <- intersect(par.columns, data %>% colnames)
  
  # outliers for each target, subject and condition
  data %<>%
    group_by(across(all_of(aux.cols))) %>%
    # IQR calculation
    mutate(low.s.d = quantile(logRT)[2] - 1.5*IQR(logRT),
           upp.s.d = quantile(logRT)[4] + 1.5*IQR(logRT)) %>%
    # evaluate whether the value belongs to the IQR
    mutate(OutlierDist = logRT < low.s.d |
                         logRT > upp.s.d)
  
  # outliers for each subject and condition (across targets)
  data %<>%
    group_by(across(all_of(setdiff(aux.cols, "Distance")))) %>%
    
    mutate(low.s = quantile(logRT)[2] - 1.5*IQR(logRT),
           upp.s = quantile(logRT)[4] + 1.5*IQR(logRT)) %>%
    
    mutate(OutlierSubj = logRT < low.s |
                         logRT > upp.s)
  
  # remove unwanted columns
  data %<>%
    select(-low.s.d, -upp.s.d, -low.s, -upp.s) %>%
    select(-logRT)

  # save results
  cat("Saving... ")
  data %>% write_csv(file = aux.fullpath) #, progress = FALSE)
  cat("DONE!\n")
  
}