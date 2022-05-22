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

# Analysis parameters
par.first_n_reversals_discarded <- 2
par.alpha <- 0.05

# Load data
data_stair <- here("data", "data_exp1_staircases.csv") %>%
  read_csv() %>%
  mutate(Exp = "1") %>%
  rbind(here("data", "data_exp2_staircases.csv") %>%
          read_csv() %>%
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
info_blocks <- data_stair %>% ungroup() %>% distinct(Condition, Block) %>%
  arrange(Condition, Block)

# Conditions performed by each subject (48 subjects x 4 combinations of
# conditions, minus 5 rows excluded due to reversals out of range = 187 rows)
info_conds_subjects <- data_stair %>%
  group_by(Subject, Condition, Block, Start) %>% 
  summarise(n = n(), .groups = "drop") %>%
  group_by(Subject, Condition, Start) %>%
  summarise(n= n(), .groups = "drop_last")

# Summary of blocks performed by each subject
info_conds_subjects_summary <- info_conds_subjects %>%
  group_by(Subject) %>%
  summarise(n = sum(n))

# 278 blocks completed (48 subjects x 6 blocks, minus 10 excluded blocks due to
# reversals out of range)
info_conds_subjects_summary %$% sum(n)


# There were four conditions (in the table, Condition means Staircase Type)
# Simple staircases had 4 repetitions, dual had 2
info_conds <- info_conds_subjects %>%
  group_by(Condition, Start, n) %>% 
  distinct(Condition, Start, n)

# Some conditions had some data excluded, hence some cases appear duplicated 
# with a lower n...
info_conds %>% arrange(Condition, Start, n)

# ...the experimental design was this:
info_conds %>% group_by(Condition, Start) %>% summarize(n = max(n), .groups = "keep")


# --- Here starts data extraction ---

# Grouping at the lowest level -includes StairId for dual staircases
data_stair %<>% group_by(Exp, Subject, Condition, Block, Start, StairId)

# How many groups: 370 (48 subjects x 8 staircases each - 14 staircases excluded)
data_stair %>% summarise(n=n(), .groups = "drop_last")

# Filter reversals. There are 4437 reversals: 370 staircases x 12 reversals each,
# minus three reversals lost in the way)
data_reversals <- data_stair %>% filter(Reversal > 0) 

# Lost reversals: S01 and S66
data_reversals %>%
  # ignore first n reversals (see below)
  filter(Reversal > par.first_n_reversals_discarded) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  filter (n != 10)

# Mean reversal (i.e. single PSE) for each Subject and Block
stat_reversals <- data_reversals %>%
  # filter first n reversals as they must be discarded for PSE calculation
  filter(Reversal > par.first_n_reversals_discarded) %>%
  # filter reversals with outlying RTs  
  filter(!(OutlierDist | OutlierSubj)) %>%
  # finally obtain mean, sd, etc.
  summarise(mean = mean(Distance),
              sd = sd(Distance),
               n = n(), .groups = "keep")

# Lost + discarded reversals
stat_reversals %>% filter (n != 10)

# Collapse dual-near and dual-far into a single condition: dual
# (Should check that there are not significant differences)
stat_reversals %<>%
  mutate(ConditionFull = if_else(Condition == "dual",
                                Condition,
                                paste(Condition, Start, sep = '-')))

# Obtain individuals PSEs for simple-near, simple-far, and dual
stat_pse <- stat_reversals %>% 
  group_by(Exp, Subject, ConditionFull) %>% 
  summarise(PSE = mean(mean), 
             sd = sd(mean),
              n = n(), .groups = "drop_last") %>%
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


# Quick and dirty plot of the effect of removing outlying reversals. Repeat
# the calculation of the PSEs without filtering RT outliers, then join with
# the PSEs that already were calculated.

# Single PSEs
stat_reversals0 <- data_stair %>% 
  filter(Reversal > 0) %>%
  # discard reversals before step halving
  filter(Reversal > par.first_n_reversals_discarded) %>%
  # obtain mean, sd, etc.
  summarise(mean = mean(Distance),
            sd = sd(Distance),
            n = n(), .groups = "keep") %>%
  # full condition column
  mutate(ConditionFull = if_else(Condition == "dual",
                                 Condition,
                                 paste(Condition, Start, sep = '-')))

# Average PSEs for simple-near, simple-far, and dual
stat_pse0 <- stat_reversals0 %>% 
  group_by(Exp, Subject, ConditionFull) %>% 
  summarise(PSE = mean(mean), 
            sd = sd(mean),
            n = n(), .groups = "drop_last") %>%
  rename(Condition = ConditionFull)

stat_pse0 <- 
  inner_join(stat_pse0, stat_pse, by = c("Exp", "Subject", "Condition"))

# plot
stat_pse0 %>% ggplot(aes(x = PSE.x, y = PSE.y)) +
  geom_point(aes(color = Condition)) +
  # identity
  geom_abline(slope=1,intercept=0, linetype= "dashed") +
  # +/- 10% lines
  geom_abline(slope=1.05,intercept=0, linetype= "dashed", color = "darkgrey") +
  geom_abline(slope=0.95,intercept=0, linetype= "dashed", color = "darkgrey") +
  # +/- 10% lines
  geom_abline(slope=1.1,intercept=0, linetype= "dashed", color = "darkgrey") +
  geom_abline(slope=0.9,intercept=0, linetype= "dashed", color = "darkgrey")
  
# correlation
stat_pse0 %$% cor.test(PSE.x, PSE.y)


