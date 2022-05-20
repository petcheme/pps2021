#### HEADER ####

# Load libraries
pacman::p_load(default,      # redefine default parameters
               here,         # path management
               ggplot2,
               magrittr,     # pipes support
               purrr,        # mapping functions
               tidyverse)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE, # prevent file locking in Windows
                      progress = FALSE, # \_ silent output
                show_col_types = FALSE, # /
                       comment = "#")

# --- Input parameters ---

my_files = c("data_exp1_psycurve.csv",
             "data_exp2_psycurve.csv",
             "data_exp1_staircases.csv",
             "data_exp2_staircases.csv")


#### ANALYSIS ####

# Start by building the file index
files_index <- 
  tibble(filename = my_files) %>%
  mutate(fullpath = here("data", filename))

# Bonus: extract method and number of experiment
files_index %<>%
  tidyr::extract(col = filename, into = c("Exp", "Method"), "data_exp(\\d)_(\\w+).csv", remove= FALSE)

# Load each file (dataset) into a nested tibble
my_data <- files_index %>%
  mutate( data = map(fullpath, ~read_csv(file = .) %>% tibble()) )

# Create Method column with values: "psy-curve", simple-near", "simple-far", and "dual"
my_data %<>%
  mutate(data = map_if(data, 
                       Method == "staircases",
                       .f = ~ .x %>% 
                         mutate(Method = if_else(Condition == "dual",
                                                 Condition, 
                                          paste0(Condition, "-", Start)), .before=2) %>%
                         select(-Condition, -Start),
                       .else = ~ .x %>% mutate(Method = "psy-curve", .before=2)
                       ))

# Outlier stats grouped by subject and method
stats <- my_data %>%
  mutate(stats = map(data, function(x) x %>%
                       group_by(Subject, Method) %>%
                       summarise(nOutDist = sum(OutlierDist),
                                 nOutSubj = sum(OutlierSubj),
                                 nOutBoth = sum(OutlierSubj | OutlierDist),
                                 n = n(), .groups = "drop") %>%
                       tibble() )) %>%
  select(Exp, stats) %>%
  unnest(stats)

# Check that everything is going well (no data lost in the way)

# number of datapoints accounted for in stats
stats %$% sum(n)
# the same in my_data
my_data %>% mutate(n = map_dbl(data, function(x) nrow(x))) %$% sum(n)

# Stats grouped by Subject
stats_subject <- stats %>% 
  group_by(Exp, Subject) %>% 
  summarise(nOut = sum(nOutBoth),
            nTot = sum(n),
            .groups = "drop") %>%
  mutate(prop = nOut / nTot)

# Stats grouped by Experiment
stats_exp <- stats_subject %>%
  group_by(Exp) %>%
  summarise(nOut = sum(nOut),
            nTot = sum(nTot)) %>%
  mutate(prop = nOut / nTot)

# Stats grouped by Method (within Experiment)
stats_method <- stats %>%
  group_by(Exp, Method) %>% 
  summarise(nOut = sum(nOutBoth),
            nTot = sum(n),
            .groups = "drop") %>%
  mutate(prop = nOut / nTot) %>%
  arrange(-prop)

# (some subsets of the previous grouping)
stats_method %>%
  arrange(Exp, Method) %>%
  filter(Method != "psy-curve") %>%
  group_by(Exp) %>%
  summarise(n = sum(nOut), N = sum(nTot))
