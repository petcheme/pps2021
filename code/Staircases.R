# Setup ####
# Load packages
pacman::p_load(here, tidyverse, magrittr, quickpsy, skimr, ggrepel, patchwork)

# Data loading and data wrangling ####

## Load data ####
data_stair <- here("data", "data_exp1_staircases.csv") %>%
  read_csv() %>%
  mutate(Experiment = "exp1") %>%
  rbind(here("data", "data_exp2_staircases.csv") %>%
          read_csv() %>%
          mutate(Experiment = "exp2")) %>%
  mutate(Experiment = factor(Experiment),
         ReversalAcum = ifelse(Reversal == 0, NA, Reversal),
         Subject = factor(Subject),
         RTLog = log(RT),
         DistanceLog = round(log(Distance), digits = 3)) %>%
  fill(ReversalAcum, .direction = c("down"))

skim(data_stair)

## Let's check the experimental conditions ####
data_stair %>% 
  group_by(Experiment, Distance, DistanceLog) %>%
  summarise() %>%
  ungroup() %>%
  group_by(Experiment) %>%
  mutate(Target = if_else(Experiment == "exp1", 1*row_number(), 2*row_number()),
         Speed = c(diff(Distance), NA)) %>%
  ggplot(aes(x = Target, y = DistanceLog, color = Experiment)) +
    geom_point()
      
  
  