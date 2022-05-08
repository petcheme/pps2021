# Load libraries
pacman::p_load(default,
               here, 
               forcats,
               ggplot2, 
               magrittr,
               tidyverse)

# pacman::p_load(emmeans,
#                lme4,
#                #nlme,
#                lmerTest)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Load data
data_pse <- 
  rbind(here("data", "data_exp1_psycurve.csv")  %>% read_csv() %>%
          mutate(Exp = 1, Method = "psy_curve") %>%
          select(Exp, Method, Subject, Distance, RT),
        here("data", "data_exp2_psycurve.csv")  %>% read_csv() %>%
          mutate(Exp = 2, Method = "psy_curve") %>%
          select(Exp, Method, Subject, Distance, RT),
        here("data", "data_exp1_staircases.csv") %>% read_csv() %>%
          mutate(Exp = 1, Method = if_else(Condition == "dual", "dual", paste0(Condition, "-", Start))) %>%
          select(Exp, Method, Subject, Distance, RT),
        here("data", "data_exp2_staircases.csv") %>% read_csv() %>%
          mutate(Exp = 2, Method = if_else(Condition == "dual", "dual", paste0(Condition, "-", Start))) %>%
          select(Exp, Method, Subject, Distance, RT)) %>%
    group_by(Exp, Method, Subject) %>% nest()



                  
                  