# Setup ####
# Load packages
pacman::p_load(here, tidyverse, magrittr, quickpsy, skimr, ggrepel, patchwork)

# Clear workspace
rm(list = ls())

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

data_stair_diagplot <- data_stair %>% 
  filter(ReversalAcum < 2) %>%
  group_by(Experiment, Distance, DistanceLog) %>%
  summarise(.groups = 'drop') %>%
  ungroup() %>%
  group_by(Experiment) %>%
  mutate(Target = row_number(),
         Speed = c(diff(Distance), NA))

p1 <- data_stair_diagplot %>%
  ggplot(aes(x = Target, y = DistanceLog, color = Experiment)) +
    geom_point() +
  labs(title = "Distance log vs. target number", x = "# Target", y = "Log Distance") +
  theme_minimal() +
  theme(legend.position = "top")
      
p2 <- data_stair_diagplot %>%
  ggplot(aes(x = Target, y = Speed, color = Experiment)) +
  geom_point() +
  labs(title = "Speed vs. target number", x = "# Target", y = "Speed") +
  theme_minimal() +
  theme(legend.position = "top")

p1 / p2  
  