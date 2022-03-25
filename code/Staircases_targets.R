# Setup ####
# Load packages
pacman::p_load(here, magrittr, patchwork, tidyverse)

# Rationale:
# In Exp. 1, source positions were equally spaced in a logarithmic scale, and
# transition steps were larger for distant targets, making farther (and usually
# approaching) sources faster than closer (usually receding) ones. In Exp. 2,
# transition steps were made homogeneous, and therefore, source positions were
# equally spaced in a linear scale (in a log scale, farther sources appeared
# more cluttered and closer sources, more sparse).

# Note that both experiments spanned similar ranges for source position, and
# that transition steps in Exp. 2 correspond (approximately) to the mean value
# of Exp. 1.


# Clear workspace
rm(list = ls())

# Analysis parameters
par.first_n_reversals_discarded <-2

# Data loading and data wrangling ####

## Load data ####
data_stair <- here("data", "data_exp1_staircases.csv") %>%
  read_csv() %>%
  mutate(Exp = 1) %>%
  rbind(here("data", "data_exp2_staircases.csv") %>%
          read_csv() %>%
          mutate(Exp = 2)) %>%
  mutate(        Exp = factor(Exp),
         DistanceLog = log10(Distance))

# Count accumulated reversals
data_stair %<>% group_by(Subject, Condition, Block, StairId) %>%
  mutate(ReversalAcum = (Reversal > 0) %>% cumsum(), .after = Reversal)


## Let's check the experimental conditions ####

data_stair_diagplot <- data_stair %>% 
  # In this case, we keep the trials prior to the first n reversals, as they 
  # are previous to the halving of the staircase step
  filter(ReversalAcum < par.first_n_reversals_discarded) %>%
  group_by(Exp, Distance, DistanceLog) %>%
  
  summarise(.groups = 'drop') %>%
  group_by(Exp) %>%
  mutate(Target = row_number(),                    # assuming that rows are correctly sorted
         Speed = c(diff(Distance), NA))

p1 <- data_stair_diagplot %>%
  ggplot(aes(x = Target, y = Distance, color = Exp)) +
    geom_point() + geom_line() + 
  scale_y_continuous(trans="log10")+
  labs(title = "Distance vs. target number", x = "# Target", y = "Distance [cm]") +
  theme_minimal() +
  theme(legend.position = "right")
      
p2 <- data_stair_diagplot %>%
  ggplot(aes(x = Target, y = Speed, color = Exp)) +
  geom_point() + geom_line() +
  labs(title = "Step vs. target number", x = "# Target", y = "Step [cm]") +
  theme_minimal() +
  theme(legend.position = "right")

p1 / p2

# This plot is a work in progress
p3 <- data_stair_diagplot %>%
  ggplot(aes(x = Exp, y = Distance, color = Exp)) +
  geom_point() +
  labs(title = "Bird view of the setup showing the\nposition of the sources") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,300)) +
  geom_text(aes(label = Target), nudge_x=-.1, color = "darkgrey") +
  geom_segment(aes(x = 1.1, y = 100, xend = 1.1, yend = 135), color = "darkgrey",
               arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
  geom_text(x = 1.25, y = 120, label = "step", color = "darkgrey") +
  geom_segment(aes(x = 2.1, y = 100, xend = 2.1, yend = 135), color = "darkgrey",
               arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
  geom_text(x = 2.25, y = 120, label = "step", color = "darkgrey")



  