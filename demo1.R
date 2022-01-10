pacman::p_load(here, tidyverse, magrittr)

# demo code for the initial upload

# responses
my_file <- here("data", "data_Psycurve_R_trial.csv")
data <- read_csv(my_file)

ggplot(data %>% group_by(Distance_lin, Distance_log) %>% summarise(Response = mean(Response)),
       aes(x=Distance_log, y=Response)) + geom_line()

# subjects
my_file <- here("data", "data_Reach_R.csv")
subjects <- read_csv(my_file) 

subjects %>% summarise(Reach = mean(Reach))
