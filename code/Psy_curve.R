# Setup ####
# Load packages
pacman::p_load(
here, # A Simpler Way to Find Your Files
tidyverse, # Easily Install and Load the 'Tidyverse'
magrittr, # A Forward-Pipe Operator for R
quickpsy, # Fits Psychometric Functions for Multiple Groups
skimr, # Compact and Flexible Summaries of Data
ggrepel, # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
patchwork, # The Composer of Plots
ggpubr # 'ggplot2' Based Publication Ready Plots
)

# Data loading and data wrangling ####

## Load data ####
data_psy <- here("data", "data_exp1_psycurve.csv") %>%
  read_csv() %>%
  mutate(Experiment = "exp1") %>%
  rbind(here("data", "data_exp2_psycurve.csv") %>%
          read_csv() %>%
          mutate(Experiment = "exp2")) %>%
  mutate(Experiment = factor(Experiment),
         Subject = factor(Subject),
         RTLog = log(RT),
         DistanceLog = log(Distance)) %>%
  filter(!OutlierDist | !OutlierSubj)

skim(data_psy)
## Summarize responses and response times accros subject and distances ####
# ---
# NACHO2021 - Considerar la posibilidad de utilizar GLMEM en lugar de quickpsy y después
# modelos sobre los parámetros
# ---
data_psy.summ <- data_psy %>% 
  group_by(Experiment, Subject, Target, Distance, DistanceLog) %>% 
  summarise(m_RT = mean(RT), 
            sd_RT = sd(RT),
            m_RTLog = mean(RTLog), 
            sd_RTLog = sd(RTLog), 
            n = n(), 
            n_Reach = sum(Response),
            .groups = 'drop')

skim(data_psy.summ)

# Psychometric curve fitting ####

# One psychometric curve to all the data (global fit)
fit.global <- quickpsy(data_psy.summ, Distance, n_Reach, n, 
                       grouping = .(Experiment), 
                       B = 1000, log = TRUE, fun = logistic_fun) 

# Use this one to regenerate the 
# One psychometric curve fit to each subjet's data (subject fit)
# fit.subject <- quickpsy(data_psy.summ, Distance, n_Reach, n, 
#                         grouping = .(Experiment, Subject), 
#                         B = 1000, log = TRUE, fun = logistic_fun) 

fit.subject <- quickpsy(data_psy.summ, Distance, n_Reach, n, 
                        grouping = .(Experiment, Subject), 
                        B = 100, #Just for saving, comment otherwise
                        # bootstrap = "none", #Uncomment if you are not interested on calculating the CIs
                        log = TRUE, fun = logistic_fun) 

# Now I save the individual PSEs
indivPSEs <- fit.subject$thresholds %>%
  select(-prob) %>% 
  mutate(Experiment = as.numeric(str_sub(Experiment,-1)),
         n = 1,
         Condition = "psy_curve") %>%
  rename(Exp = Experiment, 
         PSE = thre,
         PSE_ci_sup = thresup,
         PSE_ci_inf = threinf) %>%
  relocate(Condition, .after = Subject)
  
indivPSEs %>% write_csv(here("./data/data_PSE_psy_curve.csv"))

# Load reaching distance data
reach.df <- here("data", "data_subjects.csv") %>%
  read_csv() %>% 
  select(all_of(c("Experiment", "Subject", "Reach"))) %>%
  filter(!(Subject %in% c("S15", "S64"))) %>%
  mutate(Reach = Reach + 20,
         Experiment = factor(if_else(Experiment==1, "exp1", "exp2")),
         Subject = factor(Subject))

reach.df %>% group_by(Experiment) %>%
  summarise(mean_Reach = mean(Reach)) %$% mean_Reach

# ---
# NACHO2021 - Acá estaba lo de sacar el S01 y el S07 que deberíamos ver por qué lo hacíamos
# ---

# Normalize the threshold (PSE) using the subject's own reaching distance
thresholds.df <- fit.subject$thresholds %>% 
  inner_join(reach.df, by = c("Experiment", "Subject")) %>%
  mutate(threnormReach = thre/Reach)
  
# Response time fitting ####

# Logistic function first derivative
logisticPDF <- function(par, x)
{
  mu <- par[1]
  s <- par[2]
  k <- par[3]
  a <- par[4]
  rhat <- a + k * s * exp(-(x - mu)*s) / ((1+exp(-(x - mu)*s))^2)
  # Latex code of the equation: a+\frac{k s e^{(x-\mu)*s}}{(1+e^{(x-\mu)*s})^2} 
  return(rhat)
}

# Optimize function
optim_logisticPDF <- function(par, x, r)
{
  rhat <- logisticPDF(par, x)
  # Latex code of the equation: a+\frac{k s e^{(x-\mu)*s}}{(1+e^{(x-\mu)*s})^2} 
  return(sum((r - rhat)^2)) # square of the residual 
}

# Summarizing and fitting LogRT
fit.logRT <- data_psy %>% 
  group_by(Experiment, Distance, DistanceLog) %>%
  summarise(m_RTLog = mean(RTLog),
            .groups = 'drop') %>%
  group_by(Experiment) %>%
  nest() %>%
  mutate(fit = map(data, ~optim(c(4.6, 10, .1, -.2), optim_logisticPDF, method="BFGS", control=list(reltol=1e-9),
                                x=.x$DistanceLog, r=.x$m_RTLog))) %>%
  mutate(par = map(fit, ~ .x$par %>%
                       as.list %>% 
                       set_names(c("mu", "s", "k", "a"))))
fit.logRT

# Predicted LogRT
predicted.LogRT <- fit.logRT %>% 
  mutate(xfit = map(data, ~seq(min(.x$DistanceLog),max(.x$DistanceLog),.01)),
         curve = map2(par, xfit, ~logisticPDF(c(.x$mu, .x$s, .x$k, .x$a), .y))) %>%
  select(c(Experiment, xfit, curve)) %>% 
  unnest(cols = c(xfit, curve))

# Fit parameter
fit_pars.logRT <- fit.logRT %>%
  select(-c(fit, data)) %>%
  unnest_wider(par)

# FIGURA 1 #####

# Hago un summary para plotear

experiment_names <- c(
  `exp1` = "Experiment 1",
  `exp2` = "Experiment 2"
)
pos <- position_jitter(height = 0.05, seed = 1)

fig_psychoCurve <- function(Exp_i) {
  # Exp_i is a string containing the experiment ID ("exp1" or "exp2")
  thresholds.df %>% filter(Experiment == Exp_i) %>%
    ggplot(aes(x = thre, y = prob)) + 
    geom_point(colour="black", fill="grey", alpha = .7, pch=21, size=2.5) + 
    geom_hline(yintercept=.5, linetype = "dashed", color="grey") + 
    geom_vline(data = fit.global$thresholds %>% filter(Experiment == Exp_i), aes(xintercept = thre), linetype = "dashed", color="grey") + 
    geom_errorbarh(data = fit.global$thresholds %>% filter(Experiment == Exp_i), aes(xmax = thresup, xmin = threinf), height = .05, size = .5) +
    geom_point(data = fit.global$thresholds %>% filter(Experiment == Exp_i), aes(x = thre, y = prob),
               size = 2.5, stat="identity", color="black") +
    geom_text_repel(data = fit.global$thresholds %>% filter(Experiment == Exp_i), 
                    aes(x = thre, y = prob/2, 
                        label=sprintf("PSE=%.1f cm\nCI(%.1f %.1f)", 
                                      fit.global$thresholds %>% filter(Experiment == Exp_i) %$% thre,
                                      fit.global$thresholds %>% filter(Experiment == Exp_i) %$% threinf,
                                      fit.global$thresholds %>% filter(Experiment == Exp_i) %$% thresup)),
                    size=3, nudge_y=-0.04, nudge_x=10, direction="y", angle=0, vjust=1, segment.size = .5,
                    segment.colour = "grey") +
    stat_summary(data = fit.global$averages %>% filter(Experiment == Exp_i), aes(x = Distance, y = prob), 
                 size = 0.5, width=4, colour="grey", alpha = .7, fun.data = "mean_se", geom="errorbar") +
    stat_summary(data = fit.global$averages %>% filter(Experiment == Exp_i), aes(x = Distance, y = prob), 
                 size = 2.5, colour="grey", alpha = 1, fun.data = "mean_se", geom="point") +
    geom_line(data = fit.global$curves %>% filter(Experiment == Exp_i), aes(x = x, y = y)) +
    coord_trans(x="log10") + scale_x_continuous(breaks=c(50, 100, 150)) +
    labs(y = "No reach\nproportion") +
    theme_pubr() +
    theme(axis.title.x=element_blank(), 
          axis.text = element_text(size=8), axis.title = element_text(size=10),
          strip.background = element_blank(),
          strip.text = element_text(size=12)) 
}
fig_psychoRT <- function(Exp_i) {
  # Exp_i is a string containing the experiment ID ("exp1" or "exp2")
  data_psy.summ %>% filter(Experiment == Exp_i) %>%
    group_by(Distance) %>%
    summarise(SE_RTLog = sd(m_RTLog)/sqrt(n()),
              M_RTLog = mean(m_RTLog)) %>%
    ggplot(aes(x = Distance, y = M_RTLog)) +
    geom_point(size = 2.5, colour="grey", alpha = 1) +
    geom_errorbar(aes(ymin = M_RTLog - SE_RTLog, ymax = M_RTLog + SE_RTLog),
                  size = 0.5, colour="grey", width = 4, alpha = .7) +
    geom_line(data = predicted.LogRT %>% filter(Experiment == Exp_i), aes(x = exp(xfit), y = curve)) +
    geom_vline(data = fit_pars.logRT %>% filter(Experiment == Exp_i), aes(xintercept=exp(mu)), linetype = "dashed", color="grey") + 
    geom_text_repel(data = fit_pars.logRT %>% filter(Experiment == Exp_i), aes(x = exp(mu), y = -.25, 
                                               label=paste0(sprintf("PSE=%.1f", exp(mu)), " cm")),
                    size=3, nudge_y=-0.04, nudge_x=10, direction="y", angle=0, vjust=1, segment.size = .5, 
                    segment.colour = "grey") +
    coord_trans(x="log10") + 
    scale_y_continuous(limits = c(-0.5, 0.25)) +
    scale_x_continuous(breaks=c(50, 100, 150)) +
    labs(x = "Distance (cm)", 
         y = "log(RT)\nlog(s)") +
    theme_pubr() +
    theme(axis.text = element_text(size=9), 
          axis.title = element_text(size=10),
          strip.background = element_blank(),
          strip.text = element_blank())
}

fig1a <- fig_psychoCurve("exp1")
fig1b <- fig_psychoCurve("exp2")

fig1c <- fig_psychoRT("exp1")
fig1d <- fig_psychoRT("exp2")

fig1 <- (fig1a | fig1b) / (fig1c | fig1d) + 
  plot_layout(heights = c(1, .7)) +
  plot_annotation(tag_levels = 'a')
fig1

scale <- 1.2
ggsave(here("figures", "fig_1.png"), width = scale * 18, height = scale * 12, units = "cm")
 