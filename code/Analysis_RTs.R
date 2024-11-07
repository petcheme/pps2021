# Rationale: In this script, we fit bell curves (derivatives of a sigmoid
# function, such as the normal curve) to the response times (RTs). We expect
# to obtain an estimate of the PSE on the basis that larger RTs occur when the
# perceptual uncertainty increases, being maximum for the PSE.
#
# Note: The script is divided into several sections (foldable in RStudio IDE)
#

#### HEADER ####

# Load libraries
pacman::p_load(default,
               here, 
               forcats,
               ggplot2, ggthemes, patchwork,
               magrittr,
               tidyverse,
               lme4)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

#### 1. FITTING PROCEDURE ####

# Define fitting function

# The function must be bell-shaped, with arbitrary asymptote and amplitude
#
# Parameters:
# 
# a0: asymptote
# a:  distance from peak to asymptote (must be > 0)
# x0: position of the peak
# k:  curve width

# normal curve
gaussian_fun <-
  function(x, a0, a, x0, k) a0 + a*exp(-1/k*(x - x0)^2)

# derivative of logistic function
logistic_fun <-
  # function(x, a0, L, x0, k) a0 + L/k *( 1 + exp( -(x-x0)/k))^(-2) *exp( -(x-x0)/k)
  function(x, a0, a, x0, k) a0 + (4*a) *( 1 + exp( -8*(x-x0)/k))^(-2) *exp( -8*(x-x0)/k)
  
# Here we choose our function
fitting_fun <- gaussian_fun
#fitting_fun <- logistic_fun

# test fitting function
x <- seq(90, 110, by = 0.1) 
plot(x, fitting_fun(x, -5, 2, 100, 10))

  
# Load data
data_rt <- 
  rbind(here("data", "data_exp1_psycurve.csv")  %>% read_csv() %>%
          mutate(Exp = 1, Method = "psy_curve") %>%
          select(Exp, Method, Subject, Distance, RT, OutlierDist, OutlierSubj),
        here("data", "data_exp2_psycurve.csv")  %>% read_csv() %>%
          mutate(Exp = 2, Method = "psy_curve") %>%
          select(Exp, Method, Subject, Distance, RT, OutlierDist, OutlierSubj),
        here("data", "data_exp1_staircases.csv") %>% read_csv() %>%
          mutate(Exp = 1, Method = if_else(Condition == "dual", "dual", paste0(Condition, "-", Start))) %>%
          select(Exp, Method, Subject, Distance, RT, OutlierDist, OutlierSubj),
        here("data", "data_exp2_staircases.csv") %>% read_csv() %>%
          mutate(Exp = 2, Method = if_else(Condition == "dual", "dual", paste0(Condition, "-", Start))) %>%
          select(Exp, Method, Subject, Distance, RT, OutlierDist, OutlierSubj)) %>%
  mutate(RTlog = log10(RT), .after = "RT") %>%
  mutate(Method = fct_relevel(Method, "psy_curve", "simple-near", "dual", "simple-far")) %>%
  # group_by(Subject, Method, Distance) %>%
  # summarise(RTlog = mean(RTlog), sd = sd(RTlog)) %>%
  group_by(Exp, Subject, Method) %>%
  nest()


# Make preparations for full analysis with nls
data_rt %<>%
  mutate(start = list(map_dfc(data, function(x) 
    tibble(a0 = quantile(x$RTlog, .25, na.rm=TRUE) %>% as.numeric(),
           a  = weighted.mean(x$RTlog,    x$RTlog > quantile(x$RTlog, .90, na.rm=TRUE)) - quantile(x$RTlog, .25, na.rm=TRUE) %>% as.numeric(),
           x0 = weighted.mean(x$Distance, x$RTlog > quantile(x$RTlog, .75, na.rm=TRUE)) %>% as.numeric(),
           k  = 22*sd(x$Distance))))) %>%
  mutate(lower = map(start, function(x) c(x$a0, -10,   10, -Inf)),
         upper = list(c(15,  15, 1000,  Inf)),
         algo  = "port")

# Here begin custom settings for each subject and method (Gaussian curve)
data_rt %<>%
  mutate(
    # S03: use default algorithm
    algo = map_if(algo, Subject == "S03" & Method == "psy_curve", function(x) "default") %>% unlist(),
    # S19 and S20: discard some data
    data = map_if(data, Subject == "S19" & Method == "psy_curve", function(x) x %>% filter(Distance > 60)),
    data = map_if(data, Subject == "S20" & Method == "psy_curve", function(x) x %>% filter(Distance > 52)),
    # S04: tune starting parameters and use default method
    start = map_if(start, Subject == "S04" & Method == "simple-far", function(x) c(a0 = 0, a = 0.3, x0 = 10, k= 100000)),
    algo  = map_if(algo,  Subject == "S04" & Method == "simple-far", function(x) "default") %>% unlist(),
    # S10: use default algorithm
    algo  = map_if(algo,  Subject == "S10" & Method == "simple-far", function(x) "default") %>% unlist(),
    # S25: tune starting parameters
    start = map_if(start, Subject == "S25" & Method == "dual", function(x) c(a0 = 0, a = 0.32, x0 = 150, k= 2000)),
    lower = map_if(lower, Subject == "S25" & Method == "dual", function(x) c(-Inf, 0, 0, 0)),
    # S51: use default algorithm
    algo = map_if(algo, Subject == "S51" & Method == "dual", function(x) "default") %>% unlist(),
    )

# Run the fitting procedure
data_rt %<>% mutate(model = pmap(    list(data, start, lower, upper),
                        possibly(function(data, start, lower, upper) 
                                 nls(data    = data,
                                     # formula = RTlog ~ a0 + a*exp(-1/k*(Distance - x0)^2),
                                     formula = RTlog ~ fitting_fun(Distance, a0, a, x0, k),
                                     start   = start,
                                     lower   = lower,
                                     upper   = upper,
                                     algorithm = algo),
                                 otherwise = "error")))

# Check unsuccessful cases
data_rt %>% filter(model == "error")

#### 1b. Tweak specific fits ####

# Here we can pick some failed case and get a second look at it, then improve
# the algorithm settings in order to make the procedure successful
aux.case <- data_rt %>% filter(model == "error") %>% ungroup() %>%
  # pick first case
  slice(1)

# Get starting values
aux.start <- aux.case$start %>% unlist()
# Extract case data and compute the fitted curve for the starting values
# (This allow us to see if the heuristic is OK)
aux.data  <- aux.case %$% data[[1]] %>%
  #filter(Distance > 35) %>%
  
  # Plot the curve for the initial values (in order to check the heuristic)
  mutate(y = fitting_fun(Distance, aux.start["a0"], aux.start["a"], aux.start["x0"], aux.start["k"]))

# Plot the selected case
ggplot(aux.data) + 
  geom_point(aes(x = Distance, y = RTlog
                 # color = interaction(OutlierDist, OutlierSubj)
  )) +
  geom_line(aes(x = Distance, y = y))

# Fit the model again, here we can tweak the parameters
my_model <- nls(data    = aux.data,
                formula = RTlog ~ fitting_fun(Distance, a0, a, x0, k),
                #start   = aux.start,
                start   = c(a0=0, a=100, x0=20, k=100),
                control = nls.control(maxiter= 5000, tol = 1e-18),
                algorithm = "default",
                #lower = c(-Inf, -Inf, -Inf, -Inf), algorithm = "port"
)

# If successful, compute the fitted curve for the final values of the algorithm
aux.data %<>% mutate(y2 = predict(my_model))

# Plot the final result
ggplot(aux.data) +
  geom_point(aes(x = Distance, y = RTlog, color = interaction(OutlierDist, OutlierSubj))) +
  geom_line(aes(x = Distance, y = y)) + 
  geom_line(aes(x = Distance, y = y2), color = "darkblue")

#### 2. PLOT FITTED CURVES ####

# Plot successful cases

# First, compute predictions for each fitted model
data_rt %<>% 
  mutate(fit_curve =
    map_if(model, model != "error",
      function(m) tibble(x = seq(1,300)) %>%
        # predict makes this line independent of the funcion choice
        mutate(y = predict(m, tibble(Distance = x))),
      # allow failed fits to have predictions (NA)
      .else = function (m) tibble(x = seq(1,300)) %>%
        mutate(y = NA*x) 
        )) 


# Then, map subjects to row plots
table_plots <- data_rt %>%
  # nest all data by subject
  group_by(Subject) %>%
  # failed fits are included in the plot (their predictions count as NA)
  #filter(model != "error") %>%
  nest() %>% 
  # map subjects and their data to a custom plotting function
  mutate(plot = map2(Subject, data, function(subject, data)
    # attach subject id to the data
    data %>% select(Exp, Method, data, fit_curve) %>%
      mutate(Subject = subject, .after = Exp) %>%
      ggplot() +
      # data points
      geom_point(data = . %>% unnest(data),
                 aes(x=Distance, y=RTlog), size = .5,
                 color = "#999999") +
      facet_grid(Subject ~ Method, scales = "free_y", drop = FALSE) +
      scale_x_discrete(labels = c("Psy-curve", "Simple-near", "Dual", "Simple-far")) +
      labs(x = "Distance (cm)", y = "RT") + 
      theme_bw() + 
      theme(strip.text = element_text(size = 10),
            strip.background = element_rect(fill = "white", colour = "white", size = 1)) +
      # fitted curves
      geom_line(data = . %>% unnest(fit_curve),
                aes(x=x, y=y), color = "#D55E00")
    )) %>%
  # removes data, models, etc.
  select(-data) %>%
  # add experiment number
  mutate(Exp = if_else(Subject < "S51", 1, 2), .before = Subject)

# Optional: Now, I can map each plot to ggsave (get one PNG per plot)
# 
# # table_plots %>% filter(Subject == "S01") %$%
# table_plots %$%
#   map2(Subject, plot, function(subject, plot)
#   ggsave(filename = paste0(subject, ".png"), plot = plot,
#          width = 15, height = 4, units = "cm", dpi = 300))

# Here I change some plots in order to remove duplicate info (i will use patchwork later)
table_plots %<>% 
  # ungroup so I can use first() and last()
  ungroup() %>%
  # remove strip text for all but the first subject
  mutate(plot = map_if(plot, Subject != first(Subject),
                       function(p) p + theme(strip.text.x = element_blank())
                       )) %>%
  # remove x-axis elements for all but the last subject
  mutate(plot = map_if(plot, Subject != last(Subject),
                       function(p) p + xlab(label = NULL) + 
                         theme(axis.text.x = element_blank()) )) %>%
  # some global changes: plot margins and font size
  mutate(plot = map(plot, function(p)
    p + theme(plot.margin = unit( c(0,0,0,0),"mm"),
              text = element_text(size= 8)) + 
              guides(fill = "none") ))

# Finally, I get one single plot for everything using patchwork...
table_plots %<>% group_by(Exp) %>% 
  filter(Subject != "S07") %>%
  nest() %>%
  mutate(full_plot = map2(Exp, data,
                          function(exp, data)
                            data %$% wrap_plots(plot, ncol = 1)) )

# ...and save it in PNG format
table_plots %$% map2(Exp, full_plot, function(Exp, plot)
  ggsave(plot = plot , filename = here(paste0("figures/fig_S6_", Exp, ".png")),
         width = 18, height = 60, units = "cm"))

#### 3. PSEs COMPARISON ####

# Here comes the analysis of both types of PSEs

data_pse <-
  inner_join(
    # load PSEs from responses
    rbind(here("data", "data_PSE_staircases.csv") %>%
            read_csv() %>%
            select(Exp, Subject, Condition, PSE),
          here("data", "data_PSE_psy_curve.csv") %>%
            read_csv() %>%
            select(Exp, Subject, Condition, PSE)) %>%
      rename(Method = Condition) %>%
      mutate(Method = factor(Method)) %>%
      arrange(Exp, Subject, Method),
    # join with PSEs from RTs
    data_rt %>% filter(model != "error") %>%
      mutate(PSE = map(model, 
                       function(x) as.numeric(coef(x)["x0"]) )) %>%
      mutate(PSE.RT = unlist(PSE)) %>%
      select(Exp, Subject, Method, PSE.RT) %>%
      arrange(Exp, Subject, Method),
    by = c("Exp", "Subject", "Method")) %>%
  # some mutates
  mutate(Exp = factor(Exp)) %>%
  mutate(Method = fct_relevel(Method, "psy_curve", "simple-near", "dual", "simple-far"))

# Get Pearson's r, along with ci, t, df and p
stat_cors <- data_pse %>% group_by(Exp, Method) %>% nest() %>%
  # (here we could simplify the code in order to perform 
  # one single test to obtain one row of data)
  mutate(r      = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$estimate),
         ci.low = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$conf.int[1]),
         ci.upp = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$conf.int[2]),
         t      = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$statistic),
         df     = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$parameter),
         p      = map_dbl(data, function(x) cor.test(x$PSE, x$PSE.RT)$p.value )) %>%
  ungroup() %>%
  # adjust p-values
  mutate(p.adj  = p.adjust(p, method = "holm"),
         # also add significance labels
         signif = if_else(p.adj < 0.001, '***',
                  if_else(p.adj < 0.01,  '**',
                  if_else(p.adj < 0.05,  '*', ' (ns)')))) %>%
  arrange(Exp, Method)

# print table
stat_cors %>% select(-data)

# plot PSEs
ggplot() +
  theme_bw() +

  geom_abline(slope = 1,   intercept = 0, linetype = "dotted", color = "black") +
  geom_abline(slope = 1.2, intercept = 0, linetype = "dotted", color = "#505050") +
  geom_abline(slope = 0.8, intercept = 0, linetype = "dotted", color = "#505050") +
  geom_point(data = data_pse,
             aes(x = PSE, y = PSE.RT, shape = Exp, color = Method),
             alpha=0.6, size=2) +
  
  xlab("PSE from responses [cm]") +
  ylab("PSE from RTs [cm]") + 
  scale_color_colorblind(labels = c("Psy-curve", "Simple-near", "Dual", "Simple-far")) +

  
  labs(shape = "Experiment", color = "Method", location = "north") + 
  theme(legend.position = c(.28, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "left",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.margin = margin(0, 6, 6, 6),
        legend.title  = element_text(size=10),
        legend.text   = element_text(size=10),
        axis.text = element_text(size=11), 
        axis.title = element_text(size=12)) +

  geom_text(data = stat_cors, show.legend = FALSE,
            aes(x = c(150,150,150,150,150,150,150,150)+55,
                y = (seq(from=50, to=176, by=17)-28) %>% rev(),
                # label = paste0(signif(r,2), " (" , signif(p,2), ")" ),
                label = paste0(signif(r,3), signif ),
                hjust = "left",
                color = Method), size = 3.5) +
  # Display correlation coefficients
  annotate("point", x = c(150,150,150,150)+50,
                    y = seq(from=50, to=101, by=17)-29, shape = 17, alpha=0.7,
                    color = c("#009E73", "#56B4E9", "#E69F00", "#111111")) +
  annotate("point", x = c(150,150,150,150)+50,
                    y = seq(from=118, to=176, by=17)-29, shape = 16, alpha=0.7,
           color = c("#009E73", "#56B4E9", "#E69F00", "#111111"))

ggsave(here("figures/fig_5_big.png"), width = 14, height = 12, units = "cm")
    
