# ---- Header ----

# Load libraries
pacman::p_load(# broom,
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

# log of RT
data_stair %<>%
  mutate(logRT = log10(RT), .after = RT)


# ---- Here starts the analysis ----

data_stair %<>%
  # grouping
  group_by(Exp, Subject, CondBranch, BranchStart, Block, StairId) %>%
  # Sum previous reversals
  mutate(RevAcum = cumsum(Reversal > 0) , .after = Reversal) %>%
  select(-Response, -Distance) %>%
  # Separate before and after the first reversal
  mutate(FirstRev = if_else(RevAcum < 1, "before", "after")) %>%
  # update grouping
  group_by(FirstRev, .add = TRUE)

# check table (col RevAcum)
# data_stair %>%
#   select(-Condition, - Start, -BranchStart, -OutlierDist, -OutlierSubj) %>%
#   filter(Subject == "S01") %>% View()

# Average across trials for each block and stairid within subject and condition
data_firstrev_rt <- data_stair %>%
  summarise(mean = mean(logRT, na.rm = TRUE),
               n = n())

# check table
data_firstrev_rt %>%
  filter(Subject == "S01") %>%
  arrange(desc(CondBranch), Block, StairId)

# Extract individual data
stat_firstrev_rt <- data_firstrev_rt %>%
  # difference after - before
  summarise(diff = -diff(mean)) %>%
  # average across CondBranch within each subject
  ungroup(Block) %>%
  summarise(diff = mean(diff),
               n = n())

# Average across subjects  
mean_firstrev_rt <- stat_firstrev_rt %>%
  ungroup(Subject) %>%
  summarise(mean = mean(diff),
              sd = sd(diff),
            n=n()) %>%
  # standard error and t crit
  mutate(sem = sd / sqrt(n),
         t_crit = qt(1-par.alpha/2, n-1)) %>%
  mutate(low = mean - t_crit*sem,
         upp = mean + t_crit*sem)


# ---- Plot ----
my_jitter <- position_jitter(width = 0.1, height = 0)

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

ggplot(stat_firstrev_rt, aes(x = CondBranch, y = diff)) +
  geom_point(aes(color = BranchStart), 
             alpha = .3, 
             position = my_jitter) +
  geom_pointrange(data = mean_firstrev_rt,
                  #position = position_nudge(x=0.1),
                  aes(x = CondBranch, 
                      y = mean,
                      ymin = low,
                      ymax = upp)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkgrey") +
  # plot layout
  facet_wrap( ~ Exp, labeller = exp_labeller) +
  scale_color_manual(values = colormap) +
  scale_x_discrete(labels = c("Dual-far", "Dual-near", "Simple-far", "Simple-near")) +
  labs(x = "Method x Start", y = "Time [log10(s)] (RT diff: after 1st rev - before)", color = "Branch start") +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "white"))

ggsave(here("figures/fig_S4_1.png"), width = 18, height = 12, units = "cm")
# ---- Statistical analysis ----

comp_list <- stat_firstrev_rt %>%
  group_by(Exp, CondBranch) %>%
  select(-BranchStart) %>%
  nest(data = c(Subject, diff, n)) %>%
  mutate(t.test = map(data, function (x)
                        t.test(x$diff, alternative = "greater") %>%
                        broom::tidy())) %>%
  select(-data)

comp_list %>%
  unnest(t.test) %>%
  rename(ci.lo = conf.low, ci.up = conf.high,
         df = parameter, t = statistic, alt = alternative) %>%
  select(-method, -alt) %>%
  relocate(ci.lo, ci.up, .after = estimate) %>%
  relocate(df, .after = t) %>%
  group_by(Exp) %>%
  mutate(p.adj = p.adjust(p.value, method = "bonferroni"), .after = p.value)

