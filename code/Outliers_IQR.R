# Note: This script is a full tidyverse version of the previous one

#### HEADER ####

# Load libraries
pacman::p_load(default,      # redefine default parameters
               here,         # path management
               magrittr,     # pipes support
               tidyverse)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE, # prevent file locking in Windows
                      progress = FALSE, # \_ silent output
                show_col_types = FALSE, # /
                       comment = "#")

# Some user-defined functions
lower_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[2] - 1.5*IQR(col, na.rm = TRUE))
}

upper_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[4] + 1.5*IQR(col, na.rm = TRUE))
}



# --- Input parameters ---

# files to process
par.files = c("data_exp1_psycurve.csv",
              "data_exp2_psycurve.csv",
              "data_exp1_staircases.csv",
              "data_exp2_staircases.csv")

# which columns define groups, depending on the dataset (see below)
par.columns <- c("Subject", "Distance", "Method")


#### DATA PROCESSING ####

# Start by building the file index
files_index <- 
  tibble(filename = par.files) %>%
  mutate(fullpath = here("data", filename)) 
  
# Bonus: extract method and number of experiment
files_index %<>%
 tidyr::extract(col = filename, into = c("Exp", "Method"), "data_exp(\\d)_(\\w+).csv", remove= FALSE)

# Load each file (dataset) into a nested tibble
my_data <- files_index %>%
  mutate( map(files_index$fullpath, ~read_csv(file = .)) %>% 
            tibble(data = .))

# Create Method column with values: "psy-curve", simple-near", "simple-far", and "dual"
my_data %<>%
  mutate(data = map_if(data, 
                       Method == "staircases",
                       .f = ~ .x %>% 
                         mutate(Method = if_else(Condition == "dual",
                                                 Condition, 
                                          paste0(Condition, "-", Start)),
                                .before = 2),
                       .else = ~ .x %>% mutate(Method = "psy-curve",
                                               .before = 2)
                       ))

# Obtain group columns for each dataset -maybe this can be made simpler
my_data %<>% 
  group_by(filename) %>%
  # get column names from each dataset
  mutate( data.cols = map(data, ~ .x %>% colnames)) %>%
  # compare them with the column names defined by the user
  mutate(group.cols = intersect(par.columns, data.cols %>% unlist) %>% 
                        list() ) %>%
  # remove auxiliary columns
  select(-data.cols)

# Calculate logRT for each dataset
my_data %<>% mutate(data = map( data, ~ .x %>% 
                                  mutate(logRT = log10(RT))
                                  ))

# Calculate Distance outliers
my_data %<>% mutate(data = map( data, ~ .x %>% 
                                  # group across all condition columns
                                  group_by(across(all_of(group.cols %>% unlist))) %>%
                                  # obtain IQ range (excluding first trial of psy-curve)
                                  mutate(low.s.d = lower_iqr(logRT[ Trial != 1 | Method != "psy-curve" ]),
                                         upp.s.d = upper_iqr(logRT[ Trial != 1 | Method != "psy-curve" ])) %>%
                                  # how many points in each group (debug only)
                                  #mutate(n = sum(Trial != 1 | Method != "psy-curve")) %>%
                                  mutate(OutlierDist = logRT < low.s.d |
                                                       logRT > upp.s.d) %>%
                                  # NA data is not considered outlier, same for first trial in psy-curve
                                  mutate(OutlierDist = ifelse(is.na(OutlierDist) | (Trial == 1 & Method == "psy-curve"), FALSE, OutlierDist)) %>%
                                  select(-low.s.d, -upp.s.d)
                                  ))

# Calculate Subject outliers (same as before but ignoring column Distance)
my_data %<>% mutate(data = map( data, ~ .x %>% 
                                  # group across all condition columns except Distance
                                  group_by(across(all_of(setdiff(group.cols %>% unlist, "Distance")))) %>%
                                  # obtain IQ range (excluding first trial of psy-curve)
                                  mutate(low.s = lower_iqr(logRT[ Trial != 1 | Method != "psy-curve" ]),
                                         upp.s = upper_iqr(logRT[ Trial != 1 | Method != "psy-curve" ])) %>%
                                  # how many points in each group (debug only)
                                  #mutate(n = sum(Trial != 1 | Method != "psy-curve")) %>%
                                  mutate(OutlierSubj = logRT < low.s |
                                                       logRT > upp.s) %>%
                                  # NA data is not considered outlier, same for first trial in psy-curve
                                  mutate(OutlierSubj = ifelse(is.na(OutlierSubj) | (Trial == 1 & Method == "psy-curve"), FALSE, OutlierSubj)) %>%
                                  select(-low.s, -upp.s)
                                  ))

# Remove columns Method and logRT
my_data %<>% mutate(data = map( data, ~ .x %>% ungroup() %>%
                                  select(-any_of( c("logRT", "Method") )) 
                                  ))

# Just need to save results
my_data %$% map2(data, fullpath, write_csv)

