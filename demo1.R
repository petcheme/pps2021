library(magrittr)
library(tidyverse)

library(ggplot2)

# demo code for the initial upload

my_file <- file.path("data", "data_Psycurve_R_trial.csv")

data <- read_csv(my_file)

ggplot(data %>% group_by(Distance_lin, Distance_log) %>% summarise(Response = mean(Response)),
       aes(x=Distance_log, y=Response)) + geom_line()
