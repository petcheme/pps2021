#### HEADER ####

# Load libraries
pacman::p_load(default,
               here, 
               forcats,
               ggplot2, 
               magrittr,
               # raincloudplots,
               tidyverse)

pacman::p_load(emmeans,
               lme4,
               #nlme,
               lmerTest)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Some user-defined functions
lower_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[2] - 1.5*IQR(col, na.rm = TRUE))
}

upper_iqr <- function(col) {
  return(stats::quantile(col, probs = seq(0, 1, 0.25), na.rm = TRUE)[4] + 1.5*IQR(col, na.rm = TRUE))
}

# Analysis parameters
par.alpha <- 0.05

#### 1. DATA PREPARATION ####

# Load data
data_pse <- rbind(here("data", "data_PSE_staircases.csv") %>%
                    read_csv() %>%
                    select(Exp, Subject, Condition, PSE),
                  here("data", "data_PSE_psy_curve.csv") %>%
                    read_csv() %>%
                    select(Exp, Subject, Condition, PSE)) %>%
  rename(Method = Condition) %>%
  mutate(Method = factor(Method)) %>%
  arrange(Exp, Subject, Method)

# Relevel factor
data_pse %<>%
  mutate(Method = fct_relevel(Method, "psy_curve", "simple-near", "dual", "simple-far"))

# Check that each subject has four PSEs
data_pse %>% group_by(Subject) %>%
  summarise(n = n()) %>%
  filter(n < 4)

# Display the subjects with less than four PSEs
left_join(data_pse %>%
            group_by(Subject) %>%
            summarise(n = n()) %>%
            filter(n < 4),
          data_pse,
          by = "Subject")

# Check subjects in each experiment
data_pse %>% group_by(Exp, Method) %>%
  distinct(Subject) %>%
  summarise(n = n()) 

# Outliers analysis
data_pse %<>%
  group_by(Exp, Method) %>%
  mutate(low.iqr = lower_iqr(PSE),
         upp.iqr = upper_iqr(PSE)) %>%
  mutate(IsOutlier = PSE < low.iqr |
                     PSE > upp.iqr)

data_pse %>% filter(IsOutlier)

# Make two versions of the data: with and without you...tliers
mean_pse <- data_pse %>%
  select(-low.iqr, -upp.iqr) %>%
  group_by(Exp) %>%
  expand_grid(FilterOutliers = c(TRUE, FALSE)) %>%
  relocate(FilterOutliers, .after = Exp) %>%
  arrange(Exp, FilterOutliers) %>%
  filter(!(FilterOutliers & IsOutlier)) %>%
  group_by(Exp, FilterOutliers, Method) %>%
  select(-IsOutlier) %>%
  nest()

# Then obtain between-subject average of PSEs
mean_pse %<>%
  # mean, sd, n
  mutate(m  = map_dbl(data,   ~mean(.$PSE)),
         sd = map_dbl(data,     ~sd(.$PSE)),
         n  = map_dbl(data, ~length(.$PSE))) %>%
  # sem and confidence intervals
  mutate(sem = sd / sqrt(n),
         low = m - qt(1-par.alpha/2, n-1)*sem,
         upp = m + qt(1-par.alpha/2, n-1)*sem)

# Get mean reaching distance for each experiment
mean_reach_dist <- 
  left_join(read_csv(here("data", "data_demographics.csv")) %>%
              rename(Exp = Experiment), 
            read_csv(here("data", "data_subjects.csv")) %>%
              select(Subject, Reach),
            by = "Subject") %>%
  filter(!Excluded) %>%
  group_by(Exp) %>%
  # Data is measured from the border of the table
  mutate(Reach = Reach + 20) %>% 
  summarise(Reach_mean  = mean(Reach),
            Reach_sd    = sd(Reach),
            Reach_min   = min(Reach),
            Reach_max   = max(Reach),
            n = n(), .groups = "keep") 

#### 2. DATA PLOTTING ####

# Plot for each experiment (individual + average)
ggplot(#data = data_pse %>% filter(!IsOutlier)
       data = data_pse 
       ) +
  geom_point(aes(x = Method, y = PSE, color = IsOutlier),
             position = position_jitter(width = .1), alpha=0.55) +
  geom_errorbar(#data = mean_pse %>% filter(FilterOutliers),
                data = mean_pse %>% filter(!FilterOutliers),
                aes(x = Method, y = m, ymin = low, ymax=upp),width=0.1) +
  geom_point(#data = mean_pse %>% filter(FilterOutliers),
             data = mean_pse %>% filter(!FilterOutliers),
             aes(x = Method, y = m), color = "black", size=3, alpha=0.75) +
  geom_hline(data = mean_reach_dist,
             aes(yintercept = Reach_mean), linetype = "dashed") + 
  facet_wrap(~Exp) +
  scale_y_continuous(trans = "log10", breaks = c(60,90,135)) +
    theme_test()

#### 3. SINGLE-EXPERIMENT MODELS ####

# First, nest the data
data_pse_nested <- mean_pse %>%
  select(Exp, FilterOutliers, Method, data) %>%
  unnest(data) %>%
  group_by(Exp, FilterOutliers) %>%
  nest()

# Then, apply a linear model to each version of the data
model_analysis <- data_pse_nested %>%
  mutate(model = map(data,
                     function(x) x %>% lmer(formula = PSE ~ Method + (1 | Subject)))) %>%
  mutate(anova   = map(model, function(x) anova(x)),
         emmeans = map(model, function(x) emmeans(x, "Method")))

# Print on screen, pick your choice
model_analysis %$% walk(anova, print)
model_analysis %>% select(Exp, FilterOutliers, anova) %>% unnest(anova)
model_analysis %$% walk(model, compose(print, summary))
model_analysis %$% walk(emmeans, print)

#### 4. PAIRED COMPARISONS ####
  
# Multiple t-tests for each analysis

# First, define the comparisons...
comp_list <- data_pse %>% ungroup() %>%
  select(Method) %>% distinct() %$%
  expand_grid(X = Method, Y = Method) %>%
  filter(X != Y) %>%
  mutate(aux = if_else(as.character(X) > as.character(Y), paste(X,Y), paste(Y,X))) %>%
  distinct(aux, .keep_all = TRUE) %>%
  select(-aux)

# Also, add experiment and whether outliers are filtered
comp_list <-
  expand_grid(data_pse_nested %>% select(-data), comp_list)

# Add the data...
comp_list %<>%
  left_join(mean_pse %>% select(Exp, FilterOutliers, Method, data) %>% rename(X = Method),
            by = c("Exp", "FilterOutliers", "X")) %>%
  rename(data.X = data) %>%
  left_join(mean_pse %>% select(Exp, FilterOutliers, Method, data) %>% rename(Y = Method),
            by = c("Exp", "FilterOutliers", "Y")) %>%
  rename(data.Y = data)
  
# ...then evaluate each comparison
comp_list %<>% 
  # this is where the magic happens
  mutate(result = map2(data.X, data.Y,
                       # anonymous one-liner (thank to pipes)
                       function(x,y) inner_join(x,y, by = "Subject") %$%
                         t.test(PSE.x, PSE.y, paired = TRUE) %>%
                         broom::tidy())) %>%
  unnest(result) %>%
  # adjust p-values (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6099145/)
  group_by(Exp, FilterOutliers) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "holm")) %>%
  # remove useless columns
  select(-method, -alternative, -data.X, -data.Y) %>%
  # rename long names
  rename(df = parameter,
         t  = statistic) %>%
  # finally, sort columns the way I like
  relocate(p.value, .before = p.value.adj) %>%
  relocate(t, df,   .before = p.value)

# add significance labels
comp_list %<>%
  mutate(Signif = 
    # stats::symnum symbolically encodes a vector
    symnum(p.value.adj, 
           cutpoints = c(0,
                         0.0001, 0.001, 0.01, 0.05, 1),
           symbols   = c("****", "***", "**", "*", "ns")) %>% as.character())

# calculate effect sizes
comp_list %<>%
  # obtain s.d. of differences
  mutate(sd_diff = estimate / t*sqrt(df+1), .before = "t") %>%
  mutate(d_z = estimate / sd_diff, .before = "t")

# print table with all the stats
comp_list %>% rename(FiltOut = FilterOutliers) %>% print(n=100)
  
#### 4b. t-tests by hand (for checking the results) ####

aux <- 
inner_join(data_pse %>% filter(Method == "simple-far"),
           data_pse %>% filter(Method == "simple-near"),
           by = c("Subject", "Exp"))

aux %>%
  filter(Exp == 2) %$%
  t.test(x=PSE.x, y = PSE.y, paired = TRUE)

# also check effect sizes
aux %>% mutate( diff = PSE.x - PSE.y) %>%
  summarise(mean_diff = mean(diff),
              sd_diff =   sd(diff)) %>%
  mutate(d = mean_diff / sd_diff)


#### 5 EMMEANS ANALYSIS ####

# pairwise contrasts with emmeans (see https://benwhalley.github.io/just-enough-r/contrasts-lmer.html)
model_analysis$model[[1]] %>%
  emmeans("Method") %>%
  contrast(method = "pairwise",
           #method = "tukey"
           #method = "trt.vs.ctrl"
           adjust = "none") %>% 
  broom::tidy() %>% 
  # arrange(adj.p.value)
  arrange(p.value)

# the same with the data
comp_list %>% rename(FiltOut = FilterOutliers) %>%
  filter(Exp == 1, !FiltOut) %>% arrange(p.value)

# -- another method (with the data) --
# check missing data
model_analysis$data[[1]] %>%
  group_by(Subject) %>%
  summarise(n = n()) %>% filter( n < 4)

# use the method after filtering subjects with missing data
model_analysis$data[[1]] %>% 
  filter(Subject != "S07") %$% 
  pairwise.t.test(PSE, Method, 
                  p.adjust.method = "none",
                  paired = TRUE)
# --

# effect sizes
model_analysis$model[[1]] %>%
  emmeans("Method") %>%
  eff_size(method = "pairwise",
           sigma = model_analysis$model[[1]] %>% sigma(),
           #edf = Inf
           edf = 10
           )

#### 6. FULL MODEL FOR BOTH EXPERIMENTS ####

# (This is valid with outliers present or removed)
#
# There is an effect of Method (expected) and of Experiment (responses seems
# shifted up in Exp. 2), but the interaction is not significant (both
# experiments display the same effect)
  
# The coefficient associated to Exp seems pretty close to the distance from the
# subject to the table, could that be the cause of the difference across
# experiments (we applied the correction twice instead of once?)

data_pse %>%
  # filter(!IsOutlier) %>%  
  lmer(formula = PSE ~ Method*Exp + (1 | Subject)) %>%
  anova()

data_pse %>%
  filter(!IsOutlier) %>%  
  lmer(formula = PSE ~ Method*Exp + (1 | Subject)) %>%
  anova()
