# Load libraries
pacman::p_load(here, 
               ggplot2, 
               magrittr, 
               tidyverse)

# Clear workspace
rm(list = ls())

# Analysis parameters
par.first_n_reversals_discarded <- 2
par.alpha <- 0.05

# Load data
data_stair <- here("data", "data_exp1_staircases.csv") %>%
  read_csv(lazy = FALSE) %>%
  mutate(Exp = "1") %>%
  rbind(here("data", "data_exp2_staircases.csv") %>%
          read_csv(lazy = FALSE) %>%
          mutate(Exp = "2")) %>%
  mutate(Exp     = factor(Exp),
         Subject = factor(Subject)) %>%
  relocate(Exp, .before = Subject) %>%
  mutate(StairId = factor(StairId))


# --- Some information about the dataset ---

# How many subjects: 48
info_subjects <- data_stair %>% ungroup() %>% distinct(Exp, Subject)

# Each subject provided six blocks (dual blocks had two staircases each,
# hence each subject performed eight staircases)
info_blocks <- data_stair %>% ungroup() %>% distinct(Condition, Block)

# Conditions performed by each subject (48 subjects x 4 combinations of conditions = 192 rows)
info_conds_subjects <- data_stair %>%
  ungroup() %>%
  group_by(Subject, Condition, Block, Start) %>% 
  summarise(n = n(), .groups = "keep") %>%
  group_by(Subject, Condition, Start) %>%
  summarise(n= n())

info_conds_subjects2 <- info_conds_subjects %>%
  group_by(Subject) %>%
  summarise(n = sum(n))

# There were four conditions (in the table, Condition means Staircase Type)
# Simple staircases had 4 repetitions, dual had 2
info_conds <- info_conds_subjects %>% ungroup() %>% 
  group_by(Condition, Start, n) %>% 
  distinct(Condition, Start, n)


# --- Here starts data extraction ---

# Grouping at the lowest level -includes StairId for dual staircases
data_stair %<>% group_by(Exp, Subject, Condition, Block, Start, StairId)

# How many groups: 376 (48 subjects x 8 staircases)
data_stair %>% summarise(n=n())

# Filter reversals. There are 4509 reversals: 376 blocks x 12 reversals each,
# minus three reversals lost in the way)
data_reversals <- data_stair %>% filter(Reversal > 0) 

# Mean reversal for each Subject and Block
stat_reversals <- data_reversals %>%
  # filter first n reversals as they must be discarded for PSE calculation
  filter(Reversal > par.first_n_reversals_discarded) %>%
  summarise(mean = mean(Distance),
              sd = sd(Distance),
               n = n(), .groups = "keep")

# Lost reversals: S01 and S66
stat_reversals %>% filter (n != 11)

# Collapse dual-near and dual-far into a single condition: dual
# (Should check that there are not significant differences)
stat_reversals %<>%
  mutate(ConditionFull = ifelse(Condition == "dual",
                                Condition,
                                paste(Condition, Start, sep = '-')))

# Obtain individuals PSEs for simple-near, simple-far, and dual
stat_pse <- stat_reversals %>% 
  group_by(Exp, Subject, ConditionFull) %>% 
  summarise(PSE = mean(mean), 
             sd = sd(mean),
              n = n()) %>%
  rename(Condition = ConditionFull)

# Obtain mean PSEs (averaged between subjects)
mean_pse <- stat_pse %>%
  group_by(Exp, Condition) %>%
  # mean, sd, df
  summarise(  m = mean(PSE),
             sd = sd(PSE),
              n = n(), .groups = "keep") %>%
  # sem and confidence intervals
  mutate(sem = sd / sqrt(n),
         low = m - qt(1-par.alpha/2, n-1)*sem,
         upp = m + qt(1-par.alpha/2, n-1)*sem)
            
# Extract data to CSV
stat_pse %>% write_csv(here("data", "data_PSE_staircases.csv"))


# Plot for each experiment (individual + average)
ggplot(data = stat_pse) +
  geom_point(aes(x=Condition, y=PSE),
                   position = position_jitter(width = .1), alpha=0.2) +
  facet_wrap(~Exp) +
  geom_errorbar(data = mean_pse,
                aes(x = Condition, y = m, ymin = low, ymax=upp),width=0.2) +
  geom_point(data = mean_pse,
             aes(x = Condition, y = m), color = "red", size=4, alpha=0.5)




