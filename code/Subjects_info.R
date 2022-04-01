# Load libraries
pacman::p_load(here, 
               ggplot2, 
               magrittr, 
               tidyverse)

# Clear workspace
rm(list = ls())


data_subjects <- read_csv(here("data", "data_demographics.csv"),
                 comment = "#",
                 lazy = FALSE) %>%
  # This subject could not complete the experiment
  filter(Subject != "S64")

stats_subjects <- data_subjects %>%
  group_by(Experiment) %>%
  summarise(n = n(),
            nFemale   = sum(Gender == "F"),
            nMusician = sum(`Years of Musical Education` > 0),
            nHearProb = sum(str_to_upper(`Hearing Problems`) != "NO"),
            Age_mean  = mean(Age),
            Age_sd    = sd(Age),
            Age_min   = min(Age),
            Age_max   = max(Age))
