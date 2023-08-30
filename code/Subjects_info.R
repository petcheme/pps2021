#### HEADER ####

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

#### GROUP DEMOGRAPHICS ####

# Load data
data_demographics <- read_csv(here("data", "data_demographics.csv"))

# Report excluded subjects, then exclude them
data_demographics %>%
  filter(Excluded == TRUE) %>%
  select(Experiment, Subject, Gender, Age, Excluded, `Exclusion Motive`)

data_demographics_excluded <- data_demographics %>%
  filter(Excluded == TRUE)

data_demographics %<>% filter(Excluded == FALSE) %>%
  group_by(Experiment)

# Obtain summary statistics
stats_demographics <- data_demographics %>%
  summarise(n = n(),
            n_Fem      = sum(Gender == "F"),
            n_RHand    = sum(`Dominant Hand` == "R"),
            n_Mus      = sum(`Years of Musical Education` > 0),
            n_HearProb = sum(str_to_upper(`Hearing Problems`) != "NO"),
            n_VisiProb = sum(str_to_upper(`Vision Problems`) != "NO"),
            Age_mean  = mean(Age),
            Age_sd    = sd(Age),
            Age_min   = min(Age),
            Age_max   = max(Age))

stats_demographics

# Hearing problems
hearing <-
data_demographics %>%
  filter(str_to_upper(`Hearing Problems`) != "NO") %>%
  select(Experiment, Subject, Gender, Age, `Hearing Problems`)

# Vision problems
vision <-
data_demographics %>%
  filter(str_to_upper(`Vision Problems`) != "NO") %>%
  select(Experiment, Subject, Gender, Age, `Vision Problems`)

recruitment <- data_demographics %>% group_by(Recruitment) %>%
  summarise(n())

#### COMPARISONS ACROSS GROUPS ####

# Age comparison

# t-test
age.t.test <- data_demographics %>% 
  t.test(data = ., Age ~ Experiment)

age.t.test %>%
  broom::tidy()

# effect size
age.effsize <- age.t.test %$%
  abs((estimate[1] - estimate[2]) / stderr)

# plot
exp_names <- list(
  '1'="Experiment 1",
  '2'="Experiment 2"
)

exp_labeller <- function(variable, value){
  return(exp_names[value])
}

data_demographics %>% 
  group_by(Experiment) %>% 
  ggplot(aes(x=Age, fill=factor(Experiment))) +
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  facet_wrap(~Experiment, labeller = exp_labeller) +
  geom_vline(data = stats_demographics, 
             aes(xintercept = Age_mean), 
             linetype = "dashed") +
  labs(x = "Age (years)", y = "Counts") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "white"))

ggsave(here("figures/fig_S1_1.png"), width = 18, height = 9, units = "cm")

# Reaching distance comparison

# load data
data_reach <- read_csv(here("data", "data_subjects.csv")) %>%
  # S13 has a significantly smaller value
  # filter(Subject != "S13") %>%
  # Filter excluded subjects
  filter(!(Subject %in% (data_demographics_excluded %$% Subject))) %>%
  mutate(Experiment = factor(Experiment)) %>%
  rename(Exp = Experiment) %>%
  group_by(Exp)

# stats
stats_reach <- data_reach %>%
  summarise(Reach_mean = mean(Reach),
            Reach_sd   = sd(Reach),
            Reach_min  = min(Reach),
            Reach_max  = max(Reach),
            n = n())
  
# t-test
reach.t.test <- data_reach %>% 
  t.test(data = ., Reach ~ Exp)

reach.t.test %>%
  broom::tidy()

# effect size
reach.effsize <- reach.t.test %$%
  abs((estimate[1] - estimate[2]) / stderr)

# plot
data_reach %>%
  ggplot(aes(x=Reach, fill=Exp)) +
  geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  facet_wrap(~ Exp, labeller = exp_labeller) +
  geom_vline(data = stats_reach, 
             aes(xintercept = Reach_mean), 
             linetype = "dashed") +
  labs(x = "Maximum reaching distance (cm)", y = "Counts") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "white"))

ggsave(here("figures/fig_S1_2.png"), width = 18, height = 9, units = "cm")

