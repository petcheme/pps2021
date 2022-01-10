# Setup ####
# Load packages
pacman::p_load(here, tidyverse, magrittr, quickpsy, skimr, ggrepel, patchwork)

# Data loading and data wrangling ####

## Load data ####
data_psy <- here("data", "data_exp2_psycurve.csv") %>%
  read_csv() %>%
  mutate(RTLog = log10(RT))

skim(data_psy)

## Summarize responses andresponse times accross subject and distances ####
# ---
# NACHO2021 - Considerar la posibilidad de utilizar GLMEM en lugar de quickpsy y después
# modelos sobre los parámetros
# ---
data_psy.summ <- data_psy %>% 
  group_by(Subject, Target, Distance, DistanceLog) %>% 
  summarise(m_RT = mean(RT), 
            sd_RT = sd(RT),
            m_RTLog = mean(RTLog), 
            sd_RTLog = sd(RTLog), 
            n = n(), 
            n_Reach = sum(Response),
            .groups = 'drop')

skim(data_psy.summ)

# Psychometruc curve fitting ####

# Ajuste de la curva psicometrica global
fit.global <- quickpsy(data_psy.summ, Distance, n_Reach, n, B = 1000, log = TRUE, fun = logistic_fun) 

# Ajuste de la curva psicometrica por sujeto
fit.subject <- quickpsy(data_psy.summ, Distance, n_Reach, n, grouping = .(Subject), B = 1, log = TRUE, fun = logistic_fun) 

# Cargo los datos
reach.df <- here("data", "data_exp2_subjects.csv") %>%
  read_csv()

# Sacamos a los participantes S01 y S07
reach.df <- subset(reach.df, Subject!="S07")
reach.df <- subset(reach.df, Subject!="S01")
reach.df$Subject <- factor(reach.df$Subject)
# ---
# NACHO2021 - Esto estaría bueno recordar por qué.
# ---

# Normalizamos el PSE con el reach de cada sujeto
fit.subject$thresholds <- merge(fit.subject$thresholds, reach.df, by.x="Subject")
fit.subject$thresholds$threnormReach <- fit.subject$thresholds$thre / fit.subject$thresholds$Reach

# FITEO DE LOS RESPONSE TIMES ####

# Hacemos un df para fitear los RTs
data_psy.fitting <- data_psy %>% 
  group_by(Distance, DistanceLog) %>%
  summarise(m_RTLog = mean(RTLog))

# Variables para el ajuste
x <- data_psy.fitting$DistanceLog
r <- data_psy.fitting$m_RTLog

# Derivada de la funcion logistica para el ajuste
f_logisticPDF <- function(par)
{
  mu <- par[1]
  s <- par[2]
  k <- par[3]
  a <- par[4]
  rhat <- a + k * s * exp(-(x - mu)*s) / ((1+exp(-(x - mu)*s))^2)
  sum((r - rhat)^2)
}

# Corro el ajuste
fit.logRT <- optim(c(4.6, 10, .1, -.2), f_logisticPDF, method="BFGS", control=list(reltol=1e-9))
fit.logRT$par

fit.logRT$fit$x.fit <- seq(min(x),max(x),.01)
fit.logRT$fit$curve <- fit.logRT$par[4] + fit.logRT$par[3] * fit.logRT$par[2] *
  exp(-(fit.logRT$fit$x.fit - fit.logRT$par[1])*fit.logRT$par[2]) / 
  ((1+exp(-(fit.logRT$fit$x.fit - fit.logRT$par[1])*fit.logRT$par[2]))^2)

fit.logRT$fit <- as.data.frame(fit.logRT$fit)

plot(data_psy.fitting$DistanceLog, data_psy.fitting$m_RTLog)
lines(fit.logRT$fit$x.fit,fit.logRT$fit$curve,col="green")

# FIGURA 1 #####

# Hago un summary para plotear

pos <- position_jitter(height = 0.05, seed = 1)
fig1a <- ggplot(data = fit.subject$thresholds, aes(x = thre, y = prob)) + 
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
  coord_trans(x="log10") + scale_x_continuous(breaks=c(50, 100, 150)) +
  ylab("No reach\nproportion") +
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(size=8), axis.title = element_text(size=10))
fig1a

fig1b <- ggplot(data_psy, aes(x = Distance, y = RTLog)) +
  geom_vline(xintercept=exp(fit.logRT$par[1]), linetype = "dashed", color="grey") + 
  stat_summary(size = 0.5, colour="grey", alpha = .7, fun.data = "mean_se", geom="errorbar") +
  stat_summary(size = 2.5, colour="grey", alpha = 1, fun.data = "mean_se", geom="point") +
  geom_line(data = fit.logRT$fit, aes(x = exp(x.fit), y = curve)) +
  geom_text_repel(data = as.data.frame(fit.logRT$par[1]), aes(x = exp(`fit.logRT$par[1]`), y = .06, 
                                                              label=sprintf("PSE=%.1f cm\nCI(%.1f %.1f)", 
                                                                            exp(fit.logRT$par[1]),
                                                                            98.4, 104.7)),
                  size=3, nudge_y=0, nudge_x=-40, direction="y", angle=0, vjust=1, segment.size = .5, 
                  segment.colour = "grey") +
  coord_trans(x="log10") + scale_x_continuous(breaks=c(50, 100, 150)) +
  xlab("Distance (cm)") + ylab("log(RT)\nlog(s)") +
  theme(axis.text = element_text(size=9), axis.title = element_text(size=10))

fig1b

fig1 <- fig1a / fig1b + plot_layout(widths = c(1, .7))
fig1

scale <- 1
ggsave(here("figures", "fig_1.png"), width = scale * 9, height = scale * 12, units = "cm")

# FIGURA 1 INSERT ####

# Insert para la FIG1
pos <- position_jitter(width = 0.15, seed = 1)
fig1a_insert <- ggplot(data = fit.subject$thresholds, aes(x =1, y = threnormReach)) + 
  geom_point(colour="black", fill="grey", position = pos, alpha = .7, pch=21, size=1.5) + 
  #geom_(colour="black", fill="grey", alpha = .7, pch=21, size=.5) +
  stat_summary(size = 2.5, fun.data = "mean_se", geom="point", colour="black") + 
  stat_summary(size = .5, fun.data = "mean_se", geom="errorbar", width=.5) +
  ylab("PSE/reach") + xlim(c(.7,1.3))  +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=9), axis.title = element_text(size=10))
fig1a_insert

scale <- 1
ggsave("Figs/fig_1_insert.png", width = scale * 3, height = scale * 4, units = "cm")

