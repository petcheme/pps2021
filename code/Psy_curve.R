# Setup ####
# Load packages
pacman::p_load(here, tidyverse, magrittr, quickpsy, skimr, ggrepel, patchwork, ggpubr)

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
         DistanceLog = log(Distance))

skim(data_psy)
## Summarize responses and response times accros subject and distances ####
# ---
# NACHO2021 - Considerar la posibilidad de utilizar GLMEM en lugar de quickpsy y después
# modelos sobre los parámetros
# ---
data_psy.summ <- data_psy %>% 
  group_by(Experiment, Target, Distance, DistanceLog) %>% 
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
fit.global.2 <- quickpsy(data_psy.summ, Distance, n_Reach, n, 
                       grouping = .(Experiment), 
                       B = 1000, log = TRUE, fun = logistic_fun) 

# One psychometric curve fit to each subjet's data (subject fit)
fit.subject <- quickpsy(data_psy.summ, Distance, n_Reach, n, grouping = .(Experiment, Subject), bootstrap = 'none', log = TRUE, fun = logistic_fun) 

# Load reaching distance data
reach.df <- here("data", "data_exp1_subjects.csv") %>%
  read_csv() %>% 
  mutate(Experiment = 'exp1') %>%
  rbind(here("data", "data_exp2_subjects.csv") %>%
          read_csv() %>%
          mutate(Experiment = 'exp2')) 

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

pos <- position_jitter(height = 0.05, seed = 1)
fig1a <- ggplot(data = thresholds.df, aes(x = thre, y = prob)) + 
  geom_point(colour="black", fill="grey", alpha = .7, pch=21, size=2.5) + 
  geom_hline(yintercept=.5, linetype = "dashed", color="grey") + 
  geom_vline(xintercept=fit.global$thresholds$thre, linetype = "dashed", color="grey") + 
  geom_errorbarh(data = fit.global$thresholds, aes(xmax = thresup, xmin = threinf), height = .05, size = .5) +
  geom_point(data = fit.global$thresholds, aes(x = thre, y = prob),
             size = 2.5, stat="identity", color="black") +
  geom_text_repel(data = fit.global$thresholds, aes(x = thre, y = prob/2, 
                                                    label=sprintf("PSE=%.1f cm\nCI(%.1f %.1f)", 
                                                                  fit.global$thresholds$thre,
                                                                  fit.global$thresholds$threinf,
                                                                  fit.global$thresholds$thresup)),
                  size=3, nudge_y=-0.1, nudge_x=40, direction="y", angle=0, vjust=1, segment.size = .5,
                  segment.colour = "grey") +
  stat_summary(data = fit.global$averages, aes(x = Distance, y = prob), 
               size = 0.5, width=4, colour="grey", alpha = .7, fun.data = "mean_se", geom="errorbar") +
  stat_summary(data = fit.global$averages, aes(x = Distance, y = prob), 
               size = 2.5, colour="grey", alpha = 1, fun.data = "mean_se", geom="point") +
  geom_line(data = fit.global$curves, aes(x = x, y = y)) +
  facet_grid(.~Experiment) +
  coord_trans(x="log10") + scale_x_continuous(breaks=c(50, 100, 150)) +
  ylab("No reach\nproportion") +
  theme_pubr() +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size=8), axis.title = element_text(size=10))
fig1a

fig1b <- ggplot(data_psy.summ, aes(x = Distance, y = m_RTLog)) +
  stat_summary(size = 0.5, colour="grey", alpha = .7, fun.data = "mean_se", geom="errorbar") +
  stat_summary(size = 2.5, colour="grey", alpha = 1, fun.data = "mean_se", geom="point") +
  geom_line(data = predicted.LogRT, aes(x = exp(xfit), y = curve)) +
  geom_vline(data = fit_pars.logRT, aes(xintercept=exp(mu)), linetype = "dashed", color="grey") + 
  geom_text_repel(data = fit_pars.logRT, aes(x = exp(mu), y = -.4, 
                                             label=sprintf("PSE=%.1f", exp(mu))),
                                             size=3, nudge_y=0, nudge_x=-40, direction="y", angle=0, vjust=1, segment.size = .5, 
                                             segment.colour = "grey") +
  coord_trans(x="log10") + scale_x_continuous(breaks=c(50, 100, 150)) +
  facet_grid(.~Experiment) +
  xlab("Distance (cm)") + ylab("log(RT)\nlog(s)") +
  theme_pubr() +
  theme(axis.text = element_text(size=9), axis.title = element_text(size=10))

fig1b

fig1 <- fig1a / fig1b + plot_layout(heights = c(1, .7))
fig1

scale <- 1
ggsave(here("figures", "fig_1.png"), width = scale * 9, height = scale * 12, units = "cm")
