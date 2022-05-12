# Load libraries
pacman::p_load(default,      # redefine default parameters once and 4all: https://cran.r-project.org/web/packages/default/default.pdf
               here,         # path management
               ggplot2,
               magrittr,     # pipes support
               tidyverse)

# Rationale:
# This script checks for the occurence of out-of-range reversals in the 
# Staircase method. An out-of-range reversal may occur when a subject perceives
# the target too far or too close with respect to their body (and for this
# reason even the more careful experimental design could not avoid its
# occurence). When this happens, due to the adaptive nature of the staircase
# method, the source could reach positions outside the allowed experimental
# range, making the estimation of the PSE rather difficult. In our experimental
# design, the staircase algorithm prevented the source to go outside the
# boundaries of the experimental range, making the source to get stuck in such
# boundaries. This situation can be easily detected, as the difference in
# position between two consecutive presentations is zero.
#
# This script searches such situations for each Subject, Condition and Block.
# It also calculates the number of consecutive presentations before a reversal
# was obtained, and uses several criteria to decide the exclusion of some
# blocks when they were most affected by this problem.


# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Load data

# Load all data...
data_staircases <- 
  rbind(read_csv(here("data", "data_exp1_staircases.csv")) %>% mutate(Exp = 1, .before = 1),
        read_csv(here("data", "data_exp2_staircases.csv")) %>% mutate(Exp = 2, .before = 1),
        # ...including the data discarded by this code (for reproducibility)
        read_csv(here("data", "discarded", "data_exp1_staircases.csv")) %>%
          mutate(Exp = 1, .before = 1) %>% mutate(OutlierDist = NA, OutlierSubj = NA),
        read_csv(here("data", "discarded", "data_exp2_staircases.csv")) %>%
          mutate(Exp = 2, .before = 1) %>% mutate(OutlierDist = NA, OutlierSubj = NA))

## Some useful mutates
data_staircases <- data_staircases %>%
  # Useful when plotting dual staircases
  mutate(Trial_aux = Trial + (StairId-1)*0.5) %>%
  # Add column numReversal: Useful to compute number of out-of-range
  # presentations in a row before a reversal occured, so we can check whether
  # they are "acceptable" (short) or "unnaceptable" (long).
  group_by(Subject, Condition, Block, Start, StairId) %>%
  # Here, I use rev to compute backwards cumsum (cumsum from the end towards the
  # beginning) applied to the boolean Reversal > 0... 
  mutate(numReversal_aux = rev(cumsum(rev(Reversal > 0)))) %>%
  # ...then shift values to their correct reversal number...
  mutate(numReversal = -numReversal_aux + max(numReversal_aux) + 1,
         .after = Reversal) %>% 
  # ...finally, remove aux column. This allows to label trials in batches
  # containing each reversal and the previous trials.
  select(-numReversal_aux)


# What subjects displayed PSEs out of experimental range?

# Firstly, find reversals out of range and their length (how many times the
# same position was repeated consecutively), for each Subject and Block.
reversals_out_of_range <- data_staircases %>%
  # Discard reversals before step halving.
  filter(numReversal > 2) %>% 
  group_by(Subject, Condition, Block, Start, StairId, numReversal) %>%
  # Out-of-range targets display a zero discrete derivative of Distance in
  # consecutive trials.
  summarise(ConsecutiveDistance = sum(diff(Distance) == 0),
            .groups = "drop_last") %>%
  filter(ConsecutiveDistance > 0) %>%
  # Acceptable: Up to two identical targets presented in a row.
  mutate(IsAcceptable = ConsecutiveDistance <= 1)

# Then, find Blocks with at least N unnaceptable out-of-range reversal.
subjects_out_of_range <- reversals_out_of_range %>%
  group_by(Subject, Condition, Start, StairId, Block) %>%
  summarise(ConsecutiveDistance = sum(ConsecutiveDistance),
            # One unacceptable case is enough to discard the entire block (N = 1)
            DiscardBlock = sum(IsAcceptable == FALSE) > 0)

# Display blocks to be discarded
subjects_out_of_range %>%
  filter(DiscardBlock == TRUE) 

# Same information a little more summarised
subjects_out_of_range %>%
  filter(DiscardBlock == TRUE) %>%
  ungroup() %>%
  select(-StairId, -ConsecutiveDistance, -DiscardBlock) %>%
  distinct()


# Display overall results for each subject
subjects_out_of_range %>%
  group_by(Subject) %>%
  summarise(totTrials = sum(ConsecutiveDistance),
            totBlocks = sum(DiscardBlock) )

# Display affected subjects
subjects_out_of_range %>%
  filter(DiscardBlock) %$%
  unique(Subject) 

# Add variables to the main table: DiscardBlock and ConsecutiveDistance
data_staircases %<>%
  left_join(.,
            # Deselect StairId as it is not used as a key (see next line)
            subjects_out_of_range %>% 
              ungroup() %>% select(-StairId) %>%
              distinct(Subject, Condition, Start, Block, .keep_all = TRUE),
            # Do not use StairId as key, as we want to discard entire blocks
            by = c("Subject", "Condition", "Block", "Start")) %>%
  # Fill NA values
  mutate(ConsecutiveDistance = replace_na(ConsecutiveDistance, 0),
                DiscardBlock = replace_na(DiscardBlock,    FALSE)) %>%
  # Add boolean variable showing cases with reversals out of range. Allows to
  # check all out-of-range presentations, regardless of whether they are
  # discarded (useful to see the big picture).
  mutate(PSE_out_of_range = ConsecutiveDistance > 0)

# Plot interesting cases
p.simple_stair <- data_staircases %>%
  
  # Filter by Subject (optional)
  # filter(Subject == "S62", Condition == "dual") %>%
  
  # Show highlighted subjects only
  filter(is.element(Subject, subjects_out_of_range %>%
                               filter(DiscardBlock == TRUE) %$%
                               unique(Subject))) %>%
  
  # Filter by Condition (optional)
  # filter(Condition == "simple") %>%
  
  group_by(Subject) %>%
    ggplot(aes(x = Trial_aux, y = Distance, group = interaction(Block, StairId))) +
    geom_point(aes(color = Reversal > 0, alpha = PSE_out_of_range)) + 
    geom_line(color =  "grey") +
    scale_y_continuous(trans = "log10") +
    facet_wrap(vars(Subject, Condition), ncol = 6) 

dev.new()
plot(p.simple_stair)


# Finally save data 

# (this code could be less copy-and-paste)

## First, discarded data
data_staircases %>%
  filter(Exp == 1, DiscardBlock) %>%
  select(-Exp, -Trial_aux, -numReversal, -PSE_out_of_range, -ConsecutiveDistance, -DiscardBlock) %>%
  select(-OutlierDist, -OutlierSubj) %>%
  write_csv(here("data", "discarded", "data_exp1_staircases.csv"))

data_staircases %>%
  filter(Exp == 2, DiscardBlock) %>%
  select(-Exp, -Trial_aux, -numReversal, -PSE_out_of_range, -ConsecutiveDistance, -DiscardBlock) %>%
  select(-OutlierDist, -OutlierSubj) %>%
  write_csv(here("data", "discarded", "data_exp2_staircases.csv"))

## Then, not discarded data
data_staircases %>%
  filter(Exp == 1, !DiscardBlock) %>%
  select(-Exp, -Trial_aux, -numReversal, -PSE_out_of_range, -ConsecutiveDistance, -DiscardBlock) %>%
  write_csv(here("data", "data_exp1_staircases.csv"))

data_staircases %>%
  filter(Exp == 2, !DiscardBlock) %>%
  select(-Exp, -Trial_aux, -numReversal, -PSE_out_of_range, -ConsecutiveDistance, -DiscardBlock) %>%
  write_csv(here("data", "data_exp2_staircases.csv"))


