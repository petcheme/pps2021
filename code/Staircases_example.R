#### HEADER ####

# Load libraries
pacman::p_load(default,
               here,
               magrittr,
               tidyverse,
               ggplot2, 
               ggthemes,
               patchwork, 
               ggtext)

# Clear workspace
rm(list = ls())

# Some default values
default(read_csv) <- list(lazy = FALSE,
                      progress = FALSE,
                show_col_types = FALSE,
                       comment = "#")

# Some display configuration
options(pillar.sigfig = 4)

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

fig_staircase <- function(par, colormap, ylabel) {
    
  ylabel <- ifelse(ylabel, 1,2)
  
  #### DATA MANIPULATION ####
  
  # Average PSEs #
  
  # Individual averages
  mean_pse_indiv <- data_stair %>%
    filter(Exp == par$exp) %>%
    group_by(Exp, Condition, BranchStart, Subject, Block) %>%
    filter(Reversal > 2) %>% 
    filter(!OutlierSubj, !OutlierDist) %>%
    #filter(Subject == "S01") %>%
    
    # average across reversals within each subject, condition, branch start, and block
    summarise(Dist_mean0 = mean(Distance),
              Dist_sd    =   sd(Distance),
              n = n()) %>%
    # average across condition and branch start within each subject
    summarise(Dist_mean = mean(Dist_mean0),
              Dist_sd   =   sd(Dist_mean0),
              n = n()) %>%
    
    relocate(Subject) %>%
    arrange(Subject)
  
  # Between-subjects average
  mean_pse <- mean_pse_indiv %>%
    group_by(Exp, Condition, BranchStart) %>%
    summarise(m  = mean(Dist_mean),
              sd =   sd(Dist_mean),
              n  =    n())
  
  # Filter data by staircase type # 
  
  # Simple staircases
  data_exp_simple <- data_stair %>%
    filter(Exp == par$exp, Condition == "simple") %>%
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
  
  label_list <- list("Target distance [cm]", NULL)
  
  panel1.1 <- data_panel1.1 %>%
    ggplot(aes(x=Trial, y=Distance, group=Block)) +
      geom_point(aes(color = Start, 
                       alpha = .6
                     )) +
      geom_line(aes(color = Start)) +
      # scale_color_manual(values = c("#F8766D", "#00BFC4")) +
      scale_y_continuous(trans=par$trans) +
      ylab(label_list[[ylabel]]) +
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
      geom_point(aes(color = BranchStart), alpha = .6) +
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
    geom_point(aes(color = Start), alpha = .6) + geom_line(aes(color = Start)) +
    scale_y_continuous(trans=par$trans) +
    ylab(label_list[[ylabel]]) +
    # average pse
    geom_hline(data = mean_pse %>%
                 filter(Condition == "simple"),
               aes(yintercept = m, color = BranchStart),
               linetype = "dashed") +
    # average first reversal
    geom_vline(data = par$mean_fr %>%
                 filter(Condition == "simple"),
               aes(xintercept = Trial_mean, color = BranchStart),
               linetype = "dashed")
    
    
  # Dual staircases
  panel2.2 <- data_exp_dual %>%
      ggplot(aes(x=Trial, y=Dist_mean, group = Branch)) +
      geom_point(aes(color = Branch), alpha = .6) + geom_line(aes(color = Branch)) +
      scale_y_continuous(trans=par$trans) +
      # average pse
      geom_hline(data = mean_pse %>%
                 filter(Condition == "dual"),
               aes(yintercept = m, color = BranchStart),
               linetype = "dashed") +
      # average first reversal
      geom_vline(data = par$mean_fr %>%
                 filter(Condition == "dual"),
                 aes(xintercept = Trial_mean, color = BranchStart),
                 linetype = "dashed") +
      ylab(NULL)
  
  #### 3RD ROW: Response times ####
  
  label_list <- list("Log response time [log(s)]", NULL)
  
  # Simple staircases
  panel3.1 <- data_exp_simple %>%
    ggplot(aes(x=Trial, y=RTlog_mean, group=Start)) +
    geom_point(aes(color = Start), alpha = .6) + geom_line(aes(color = Start)) +
    ylab(label_list[[ylabel]]) +
    geom_vline(data = par$mean_fr %>%
               filter(Condition == "simple"),
               aes(xintercept = Trial_mean, color = BranchStart),
               linetype = "dashed") +
    scale_y_continuous(breaks = log(seq(.7, 1.6, by = 0.1)),
                       labels = seq(.7, 1.6, by = 0.1),
                       limits = log10(c(0.66, 1.6)))
  
  # Dual staircases
  panel3.2 <- data_exp_dual %>%
    ggplot(aes(x=Trial, y=RTlog_mean, group=Branch)) +
    geom_point(aes(color = Branch), alpha = .6) + geom_line(aes(color = Branch)) +
    geom_vline(data = par$mean_fr %>%
               filter(Condition == "dual"),
               aes(xintercept = Trial_mean, color = BranchStart),
               linetype = "dashed") +
    scale_y_continuous(breaks = log(seq(.7, 1.6, by = 0.1)),
                       labels = seq(.7, 1.6, by = 0.1),
                       limits = log10(c(0.66, 1.6))) +
    ylab(NULL)
  
  #### PATCHWORK PLOTS ####
  
  theme_set(theme_bw(base_size = 8))
  theme_update(plot.subtitle = element_text(hjust = 0.5),
               legend.position = "none")
  
  my_scale_x <- function (experi, method) { 
    if (experi == 1 & method == "simple") 
      {scale_x_continuous(limits = c(1,32), breaks = c(1,seq(5,32, by=5)))}
    else if (experi == 1 & method == "dual")
      {scale_x_continuous(limits = c(1,37), breaks = c(1,seq(5,37, by=5)))}
    else if (experi == 2 & method == "simple") 
      {scale_x_continuous(limits = c(1,26), breaks = c(1,seq(5,26, by=5)))}
    else if (experi == 2 & method == "dual")
      {scale_x_continuous(limits = c(1,31), breaks = c(1,seq(5,31, by=5)))}
  }
  
  my_tag <- function (my_label, experi) {
    return (labs(tag = paste(my_label, experi, sep = "")) )
  }
  
  (panel1.1 + labs(subtitle="Simple") +
     xlab(label = "") + 
     my_tag(my_label = "a", experi = par$exp) + 
     my_scale_x(experi = par$exp, method = "simple") + 
     scale_color_manual(values = colormap) |
   panel1.2 + labs(subtitle="Dual")   +
     my_tag(my_label = "b", experi = par$exp) + 
     xlab(label = "") + 
     my_scale_x(experi = par$exp, method = "dual") + 
     scale_color_manual(values = colormap)
    ) /
  
  (panel2.1 + 
     my_tag(my_label = "c", experi = par$exp) + 
     xlab(label = "") + 
     my_scale_x(experi = par$exp, method = "simple") + 
     scale_color_manual(values =colormap) |
   panel2.2 + 
     my_tag(my_label = "d", experi = par$exp) + 
     xlab(label = "") + 
     my_scale_x(experi = par$exp, method = "dual") + 
     scale_color_manual(values = colormap)) /
  
  (panel3.1 + 
     my_tag(my_label = "e", experi = par$exp) + 
     my_scale_x(experi = par$exp, method = "simple") + 
     scale_color_manual(values = colormap) |
   panel3.2 + 
     my_tag(my_label = "f", experi = par$exp) + 
     my_scale_x(experi = par$exp, method = "dual") + 
     scale_color_manual(values = colormap) 
   ) +
    plot_annotation(paste0("Experiment ", par$exp), theme=theme(plot.title=element_text(hjust=0.5)))
  
}


# colorblind palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colormap <- cbp1[c(4,2)]

#### EXPERIMENT 1 STAIRCASE ####
# If you want to plot the invididual staircases for experiment 1 keep this par definition
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

fig_staircase_exp_1 <- fig_staircase(par, colormap, TRUE)

#### EXPERIMENT 1 STAIRCASE ####
# If you want to plot the invididual staircases for experiment 2 keep this par definition
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

fig_staircase_exp_2 <- fig_staircase(par, colormap, FALSE)

(ggplot() + labs(title = "Experiment 1") + theme_void() + theme(plot.title = element_text(size = 12, hjust = .5), plot.title.position = "plot") | 
    ggplot() + labs(title = "Experiment 2")+ theme_void() + theme(plot.title = element_text(size = 12, hjust = .5), plot.title.position = "plot")) /
  (fig_staircase_exp_1 | fig_staircase_exp_2 ) +
  plot_layout(heights = c(1,30)) +
  plot_annotation(caption = paste0("Start point of the staircase: <b style='color:",colormap[1],"'>Far</b> <b style='color:",colormap[2],"'>Near</b>"),
                  theme = theme(plot.caption =element_markdown(face = "bold", hjust = .5, size = 9)))
  
ggsave(here("figures/fig_3.png"), width = 18, height = 14, units = "cm")
