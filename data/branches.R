# Load libraries
pacman::p_load(here,
               magrittr,
               tidyverse)

## Add a column named BranchStart, indicating the position ("near" or "far") of
## the first trial of each Branch in each Block (useful for dual staircases)

# Clear workspace
rm(list = ls())

# Load data
my_data <- here("data", "data_exp1_staircases.csv") %>%
  read_csv(lazy = FALSE) %>%
  mutate(Exp = 1) %>%
  rbind(here("data", "data_exp2_staircases.csv") %>%
          read_csv(lazy = FALSE) %>%
          mutate(Exp = 2)) %>%
  relocate(Exp, .before = Subject)

# Extract levels
levels <- my_data %>% distinct (Start) %$% Start

# Create BranchStart column (avoided use of factors since they obscured the code -afaik)
my_data %<>%
  # simple staircases -> they have one single branch, the start of the branch
  #                      equals the start of the block
  # dual staircases -> the start of branch 1 is the start of the block, the
  #                    start of the other branch is the opposite
  mutate(BranchStart = if_else(Condition == "simple",
                               Start, 
                               if_else((Start == levels[1] & StairId == 1) |
                                       (Start == levels[2] & StairId == 2), 
                                       levels[1] ,
                                       levels[2])), .after = StairId)

# lastly, save results
my_data %>% 
  filter(Exp == 1) %>%
  select(-Exp) %>%
  # select(-BranchStart) %>%
  write_csv(here("data", "data_exp1_staircases.csv"))

my_data %>% 
  filter(Exp == 2) %>%
  select(-Exp) %>%
  # select(-BranchStart) %>%
  write_csv(here("data", "data_exp2_staircases.csv"))

