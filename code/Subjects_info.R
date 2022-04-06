# Load libraries
pacman::p_load(default,
               here,
               ggplot2,
               magrittr,
               tidyverse)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Load data
data_demographics <- read_csv(here("data", "data_demographics.csv"))

# Report excluded subjects, then exclude them
data_demographics %>%
  filter(Excluded == TRUE) %>%
  select(Experiment, Subject, Gender, Age, Excluded, `Exclusion Motive`)

data_demographics_excluded <- data_demographics %>%
  filter(Excluded == TRUE)

data_demographics %<>% filter(Excluded == FALSE) 


# Obtain summary statistics
stats_demographics <- data_demographics %>%

  group_by(Experiment) %>%
  summarise(n = n(),
            n_Fem     = sum(Gender == "F"),
            n_Mus     = sum(`Years of Musical Education` > 0),
            n_HearProb = sum(str_to_upper(`Hearing Problems`) != "NO"),
            n_VisiProb = sum(str_to_upper(`Vision Problems`) != "NO"),
            Age_mean  = mean(Age),
            Age_sd    = sd(Age),
            Age_min   = min(Age),
            Age_max   = max(Age))

stats_demographics

# Hearing problems
data_demographics %>%
  filter(str_to_upper(`Hearing Problems`) != "NO") %>%
  select(Experiment, Subject, Gender, Age, `Hearing Problems`)

# Vision problems
data_demographics %>%
  filter(str_to_upper(`Vision Problems`) != "NO") %>%
  select(Experiment, Subject, Gender, Age, `Vision Problems`)


