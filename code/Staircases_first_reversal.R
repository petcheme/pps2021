# This script contains the analysis of Supp. Material S1. We first calculate 
# the average number of trial for the first reversal, for each condition. Then
# we analyzed if there was any influence due to the staircase type (dual vs
# simple).

# ---- Header ----

# Load libraries
pacman::p_load(broom,
               default,
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

# Individual averages (for each staircase condition)
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

# Display table with mean values and other stats
mean_firstrev %>% ungroup() %>%
  select(-BranchStart, -Condition) %>%
  arrange(Exp, desc(CondBranch))


# ---- Plot ----
my_jitter <- position_jitter(width = 0.1, height = 0.01)

# colorblind palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colormap <- cbp1[c(3,7)]

exp_names <- list(
  '1'="Experiment 1",
  '2'="Experiment 2"
)

exp_labeller <- function(variable, value){
  return(exp_names[value])
}

ggplot(data = stat_firstrev,
       aes(x = CondBranch, 
           y = Trial_mean)) +
  # point layer
  geom_point(aes(color = BranchStart,
                 alpha = IsOutlier),
                  size = 2,
              position = my_jitter) +
  # line layer
  geom_line(aes(group = interaction(Subject,BranchStart)),
                alpha = .0025,
             linetype = "longdash") +
  # group means
  geom_pointrange(data = mean_firstrev,
                  #position = position_nudge(x=0.1),
                  aes(   x = CondBranch, 
                         y = m,
                      ymin = low,
                      ymax = upp)) +
  geom_line(data = mean_firstrev, 
            linetype = "longdash",
            #position = position_nudge(x=0.1),
            aes(x = CondBranch ,
                y = m,
                group=interaction(Exp, BranchStart))) +
  scale_color_manual(values = colormap) +
  scale_alpha_manual(values = c(.2, 1), guide = "none") +
  scale_x_discrete(labels = c("Dual-far", "Dual-near", "Simple-far", "Simple-near")) +
  # plot layout
  facet_wrap( ~ Exp, labeller = exp_labeller) +
  labs(x = "Method x Start", y = "Trial of first reversal", color = "Branch start") +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "white"))

ggsave(here("figures/fig_S3_1.png"), width = 18, height = 12, units = "cm")
# ---- Statistical analysis ----

# purr::pmap version (one single pipe mapped repeatedly over our data)
comp_list <-
  crossing(Exp = factor(c(1,2)),
           tibble(x = c(  "dual-near",   "dual-far"), 
                  y = c("simple-near", "simple-far")),
           tibble(stat_firstrev %>%
                    ungroup() %>%
                    nest(data = everything())) )
comp_list %<>%
  mutate(t.test = pmap(list(Exp, x, y, data),
                  function(my_exp, cond_x, cond_y, my_data)
                    my_data %>%
                      ungroup() %>%
                      # filter outliers, also select experiment
                      filter(!IsOutlier, Exp == my_exp) %>%
                      select(CondBranch, Subject, Trial_mean) %>%
                      # fork the pipe to make a join of the data with itself
                      {inner_join(
                        # pair data based on subject
                        dplyr::filter(., CondBranch == cond_x),
                        dplyr::filter(., CondBranch == cond_y),
                        by = "Subject")} %$%
                      # here comes t-test
                      t.test(Trial_mean.x - Trial_mean.y,
                             # single-tail test, given our hypothesis has sign
                             alternative = "greater") %>%
                      tidy()
                    ))

# Print data on screen
comp_list %>% unnest(t.test) %>%
  rename(ci.lo = conf.low, ci.up = conf.high,
    df = parameter, t = statistic, alt = alternative) %>%
  select(-data, -method, -alt) %>%
  group_by(Exp) %>%
  # adjust for multiple comparisons
  mutate(p.adj = p.adjust(p.value, method = "bonferroni"), .after = p.value) %>%
  relocate(p.value, .after = last_col()) %>%
  relocate(p.adj, .after = last_col())


# Below there is the old version (without map)

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
