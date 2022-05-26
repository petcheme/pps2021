# ---- Header ----

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

# Some user-defined functions (for outlier detection)
lower_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[2] - 1.5*IQR(col, na.rm = TRUE))
}

upper_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[4] + 1.5*IQR(col, na.rm = TRUE))
}

# Analysis parameters
par.n_reversal <- 1
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
  mutate(StairId = factor(StairId)) %>%
  mutate(CondBranch = paste0(Condition, "-", BranchStart), .after = Subject)


# ---- Here starts the analysis ----

# Average trial of first reversal
 
# Extract raw data
data_firstrev <- data_stair %>%
  filter(Reversal == par.n_reversal) %>%
  select(-Start, -Response, -StairId, -RT, -Distance, -OutlierDist, -OutlierSubj, -Reversal)

# Individual averages
stat_firstrev <- data_firstrev %>%  
  group_by(Exp, CondBranch, Condition, BranchStart, Subject) %>%
  summarise(Trial_mean = mean(Trial),
            Trial_Sd =     sd(Trial),
                   n = n(), .groups = "drop_last")

# Outlier detection
stat_firstrev %<>%
  group_by(Exp, CondBranch) %>%
  mutate(out.lo = lower_iqr(Trial_mean),
         out.up = upper_iqr(Trial_mean)) %>%
  mutate(IsOutlier = (Trial_mean < out.lo) | (Trial_mean > out.up))

# Group averages
mean_firstrev <- stat_firstrev %>% 
  group_by(Condition, BranchStart, .add = TRUE) %>%
  # filter outliers
  filter(!IsOutlier) %>%
  summarise(  m = mean(Trial_mean),
             sd =   sd(Trial_mean),
              n = n()) %>%
  mutate( sem = sd / sqrt(n),
       t_crit = qt(1-par.alpha/2, n-1)) %>%
  mutate(low = m - t_crit*sem,
         upp = m + t_crit*sem)

mean_firstrev %>% ungroup() %>%
  select(-BranchStart, -Condition) %>%
  arrange(Exp, desc(CondBranch))


# ---- Plot ----
my_jitter <- position_jitter(width = 0.1, height = 0.01)

ggplot(data = stat_firstrev,
       aes(x = CondBranch, 
           y = Trial_mean)) +
  # point layer
  geom_point(aes(color = BranchStart,
                 shape = IsOutlier),
                  size = 2,
                 alpha = 0.5,
              position = my_jitter) +
  # line layer
  geom_line(aes(group = interaction(Subject,BranchStart)),
                alpha = .0025,
             linetype = "longdash") +
  # group means
  geom_pointrange(data = mean_firstrev,
                  position = position_nudge(x=0.1),
                  aes(   x = CondBranch, 
                         y = m,
                      ymin = low,
                      ymax = upp)) +
  geom_line(data = mean_firstrev, 
            linetype = "longdash",
            position = position_nudge(x=0.1),
            aes(x = CondBranch ,
                y = m,
                group=interaction(Exp, BranchStart))) +
  # plot layout
  facet_wrap( ~ Exp) +
  ylab("Trial of first reversal") +
  xlab("Method x Start") + 
  theme_classic()


# ---- Statistical analysis ----

# Exp 1:
stat_firstrev %>%
  ungroup() %>%
  filter(!IsOutlier, Exp == 1) %>%
  select(CondBranch, Subject, Trial_mean) %>%
  {inner_join(
    dplyr::filter(., CondBranch == "dual-near"),
    dplyr::filter(., CondBranch == "simple-near"),
    by = "Subject")} %$%
  t.test(Trial_mean.x - Trial_mean.y,
         alternative = "greater")

stat_firstrev %>%
  ungroup() %>%
  filter(!IsOutlier, Exp == 1) %>%
  select(CondBranch, Subject, Trial_mean) %>%
  {inner_join(
    dplyr::filter(., CondBranch == "dual-far"),
    dplyr::filter(., CondBranch == "simple-far"),
    by = "Subject")} %$%
  t.test(Trial_mean.x - Trial_mean.y,
         alternative = "greater")

# Exp 2:
stat_firstrev %>%
  ungroup() %>%
  filter(!IsOutlier, Exp == 2) %>%
  select(CondBranch, Subject, Trial_mean) %>%
  {inner_join(
    dplyr::filter(., CondBranch == "dual-near"),
    dplyr::filter(., CondBranch == "simple-near"),
    by = "Subject")} %$%
  t.test(Trial_mean.x - Trial_mean.y,
         alternative = "greater")

stat_firstrev %>%
  ungroup() %>%
  filter(!IsOutlier, Exp == 2) %>%
  select(CondBranch, Subject, Trial_mean) %>%
  {inner_join(
    dplyr::filter(., CondBranch == "dual-far"),
    dplyr::filter(., CondBranch == "simple-far"),
    by = "Subject")} %$%
  t.test(Trial_mean.x - Trial_mean.y,
         alternative = "greater")
