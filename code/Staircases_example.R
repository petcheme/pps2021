#### HEADER ####

# Load libraries
pacman::p_load(default,
               here,
               magrittr,
               tidyverse)

pacman::p_load(ggplot2,
               patchwork)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Some display configuration
options(pillar.sigfig = 4)

# Parameters
par <- list("exp" = 1,
        "subject" = "S06",
      "max_trial" = 30,
  "simple_blocks" = c(1,2),
    "dual_blocks" = c(1),
          "trans" = "log10",
        # data from script "Staircases_first_reversal.R"
        "mean_fr" = tibble(Condition   = c("simple", "simple", "dual", "dual"),
                           BranchStart = c("near", "far", "near", "far"),
                           Trial_mean  = c(10.1, 7.17, 10.5, 8.17)))

par <- list("exp" = 2,
        "subject" = "S56",
      "max_trial" = 26,
  "simple_blocks" = c(1,2),
    "dual_blocks" = c(2),
          "trans" = "identity",
        # data from script "Staircases_first_reversal.R"
        "mean_fr" = tibble(Condition   = c("simple", "simple", "dual", "dual"),
                           BranchStart = c("near", "far", "near", "far"),
                           Trial_mean  = c(5.91, 5.5, 6.61, 6.35)))

# Load data
data_stair <- 
  rbind(here("data", "data_exp1_staircases.csv") %>% 
          read_csv() %>%
          mutate(Exp = "1"),
        here("data", "data_exp2_staircases.csv") %>%
          read_csv() %>%
          mutate(Exp = "2")) %>%
  mutate(Exp     = factor(Exp),
         Subject = factor(Subject)) %>%
  relocate(Exp, .before = Subject) %>%
  mutate(StairId = factor(StairId),
         Start   = factor(Start))

#### DATA MANIPULATION ####

# Filter experiment
data_exp <- data_stair %>%
  filter(Exp == par$exp)

# Average PSEs #

# Individual averages
mean_pse_indiv <- data_exp %>%
  group_by(Exp, Condition, BranchStart, Subject) %>%
  filter(Reversal > 2) %>% 
  #filter(Subject == "S01") %>%
  summarise(Dist_mean = mean(Distance),
            Dist_sd   =   sd(Distance),
            n = n()) %>%
  relocate(Subject) %>%
  arrange(Subject)

# Between-subjects average
mean_pse <- mean_pse_indiv %>%
  group_by(Exp, Condition, BranchStart) %>%
  summarise(Dist_mean = mean(Dist_mean),
            n = n())

# Filter data by staircase type # 

# Simple staircases
data_exp_simple <- data_exp %>%
  filter(Condition == "simple") %>%
  group_by(Start, Trial) %>%
  summarise(Dist_mean = mean(Distance),
            Dist_sd   =   sd(Distance),
            RTlog_mean = mean(log10(RT)),
            RTlog_sd   =   sd(log10(RT)),
            n = n()) %>%
  filter(Trial <= par$max_trial)

# Dual staircases
data_exp_dual <- data_stair %>%
  filter(Exp == par$exp, Condition == "dual") %>%
  
  # this mutate should not be necessary
  mutate(Branch = if_else((Start == "near" & StairId == 1) |
                          (Start == "far"  & StairId == 2),
                          "near", "far")) %>%
  
  group_by(Branch, Trial) %>%
  summarise(Dist_mean  = mean(Distance),
            Dist_sd    =   sd(Distance),
            RTlog_mean = mean(log10(RT)),
            RTlog_sd   =   sd(log10(RT)),
            n = n()) %>%
  filter(Trial <= par$max_trial)

#### FIRST ROW: Individual example ####

# Filter subject data
data_subject <- data_stair %>%
  filter(Subject == par$subject)

# Simple staircases
data_panel1.1 <- data_subject %>%
  filter(Condition == "simple", is.element(Block, par$simple_blocks))

# some testing code
# data_panel1.1 %>%
#   filter(Reversal > 2) %>% 
#   group_by(Subject, BranchStart) %>%
#   summarise(Dist_mean = mean(Distance))

panel1.1 <- data_panel1.1 %>%
  ggplot(aes(x=Trial, y=Distance, group=Block)) +
    geom_point(aes(color = Start, shape = (Reversal > 2))) +
    geom_line(aes(color = Start)) +
    # scale_color_manual(values = c("#F8766D", "#00BFC4")) +
    scale_y_continuous(trans=par$trans) +
    ylab("Target distance [cm]") +
  
    # PSEs (here I take the input data and branch the analysis)
    geom_hline(data = . %>%
                 filter(Reversal > 2) %>%
                 group_by(Subject, BranchStart) %>%
                 summarise(Dist_mean = mean(Distance)),
             aes(yintercept = Dist_mean,
                 color = BranchStart),
             linetype = "dashed") #+
  #geom_hline(yintercept = 88.97) + geom_hline(yintercept = 139.27)

data_panel1.1 %>%
  filter(Reversal > 2) %>%
  filter(!(OutlierDist | OutlierSubj)) %>%
  group_by(Subject, BranchStart) %>%
  summarise(Dist_mean = mean(Distance)) %$% Dist_mean

panel1.1

# Dual staircases
data_panel1.2 <- data_subject %>%
  filter(Condition == "dual", is.element(Block, par$dual_blocks))

panel1.2 <- data_panel1.2 %>%
  ggplot(aes(x=Trial, y=Distance, group = BranchStart)) +
    geom_point(aes(color = BranchStart)) +
    geom_line(aes(color = BranchStart)) +
    scale_y_continuous(trans=par$trans) +
    #scale_color_manual(values = c("#00BFC4", "#F8766D")) +
    ylab(NULL)  + 
  
    # PSEs (here I take the input data and branch the analysis)
    geom_hline(data = . %>%
                 filter(Reversal > 2) %>%
                 group_by(Subject, BranchStart) %>%
                 summarise(Dist_mean = mean(Distance)),
               aes(yintercept = Dist_mean,
                   color = BranchStart),
               linetype = "dashed") #+
 #geom_hline(yintercept = 131.12) + geom_hline(yintercept = 140.28)

data_panel1.2 %>%
  filter(Reversal > 2) %>%
  filter(!(OutlierDist | OutlierSubj)) %>%
  group_by(Subject, BranchStart) %>%
  summarise(Dist_mean = mean(Distance)) %$% Dist_mean

panel1.2

#### 2ND ROW: Average target position ####

# Simple staircases
panel2.1 <- data_exp_simple %>%
  ggplot(aes(x=Trial, y=Dist_mean, group=Start)) +
  geom_point(aes(color = Start)) + geom_line(aes(color = Start)) +
  scale_y_continuous(trans=par$trans) +
  ylab("Target distance [cm]") +
  # average pse
  geom_hline(data = mean_pse %>%
               filter(Condition == "simple"),
             aes(yintercept = Dist_mean, color = BranchStart),
             linetype = "dashed") +
  # average first reversal
  geom_vline(data = par$mean_fr %>%
               filter(Condition == "simple"),
             aes(xintercept = Trial_mean, color = BranchStart),
             linetype = "dashed")
  
# Dual staircases
panel2.2 <- data_exp_dual %>%
    ggplot(aes(x=Trial, y=Dist_mean, group = Branch)) +
    geom_point(aes(color = Branch)) + geom_line(aes(color = Branch)) +
    scale_y_continuous(trans=par$trans) +
    # average pse
    geom_hline(data = mean_pse %>%
               filter(Condition == "dual"),
             aes(yintercept = Dist_mean, color = BranchStart),
             linetype = "dashed") +
    # average first reversal
    geom_vline(data = par$mean_fr %>%
               filter(Condition == "dual"),
               aes(xintercept = Trial_mean, color = BranchStart),
               linetype = "dashed") +
    ylab(NULL)

#### 3RD ROW: Response times ####

# Simple staircases
panel3.1 <- data_exp_simple %>%
  ggplot(aes(x=Trial, y=RTlog_mean, group=Start)) +
  geom_point(aes(color = Start)) + geom_line(aes(color = Start)) +
  ylab("Response time [s]") +
  geom_vline(data = par$mean_fr %>%
             filter(Condition == "simple"),
             aes(xintercept = Trial_mean, color = BranchStart),
             linetype = "dashed") +
  scale_y_continuous(breaks = log10(seq(.7, 1.6, by = 0.1)),
                     labels = seq(.7, 1.6, by = 0.1),
                     limits = log10(c(0.7, 1.6)))

# Dual staircases
panel3.2 <- data_exp_dual %>%
  ggplot(aes(x=Trial, y=RTlog_mean, group=Branch)) +
  geom_point(aes(color = Branch)) + geom_line(aes(color = Branch)) +
  geom_vline(data = par$mean_fr %>%
             filter(Condition == "dual"),
             aes(xintercept = Trial_mean, color = BranchStart),
             linetype = "dashed") +
  scale_y_continuous(breaks = log10(seq(.7, 1.6, by = 0.1)),
                     labels = seq(.7, 1.6, by = 0.1),
                     limits = log10(c(0.7, 1.6))) +
  ylab(NULL)

#### PATCHWORK PLOTS ####

(panel1.1 + labs(subtitle="Simple") +
   theme_classic() +
   theme(plot.subtitle = element_text(hjust = 0.5),
       legend.position = "none") |
 panel1.2 + labs(subtitle="Dual")   +
   theme_classic() +
   theme(plot.subtitle = element_text(hjust = 0.5),
       legend.position = "none")) /

(panel2.1 + labs(subtitle="Simple") +
   theme_classic() +
   theme(plot.subtitle = element_text(hjust = 0.5),
       legend.position = "none")  |
 panel2.2 + labs(subtitle="Dual")   +
   theme_classic() +
   theme(plot.subtitle = element_text(hjust = 0.5),
       legend.position = "none")) /

(panel3.1 + labs(subtitle="Simple") +
   theme_classic() +
   theme(plot.subtitle = element_text(hjust = 0.5),
       legend.position = "none") |
 panel3.2 + labs(subtitle="Dual")   +
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"))

