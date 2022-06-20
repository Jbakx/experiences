# Multilevel logistic regression models for TIMELINE in specific population:
# The effect of all events in previous EC and GC patient on the treatment decision in the fragile 
# population of both EC and GC patients with age >= 75 and/or performance status >= 2. 

# Jeanne Bakx
# 08-06-2022 aangemaakt
# 13-06-2022 bijgewerkt

rm(list = ls())   #clear work environment

library(openxlsx)
library(ggplot2)
library(cowplot)

#load folder and dataset based on set parameters
main_folder <- "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen"

#load EC and GC results __________________________________________________________________________________________________________
folder <- paste(main_folder, "/resultaten/final/cases", sep = "")     
load(paste(folder, "/oddsratios GC and EC events in EC and GC patients.Rdata", sep = ""))

outcomes <- outcome_model

outcomes$cases    <- stringr::str_extract(outcomes$event, "(\\d)+(?=cases)")
outcomes$cases    <- paste(outcomes$cases, "cases", sep = "")
outcomes$events   <- sapply(stringr::str_split(outcomes$event, pattern = outcomes$cases), `[`, 1)
outcomes$events   <- sub("ratio_", "", outcomes$events)
outcomes$p_signif <- ifelse(outcomes$p_value < 0.001, '**', ifelse(outcomes$p_value < 0.01, '*', ''))
outcomes$cases    <- sub("([a-z])", " \\1", outcomes$cases)
outcomes$cases    <- factor(outcomes$cases, levels = c("40 cases", "30 cases", "20 cases", "10 cases", "5 cases") )
  
  
#order dataframe to match the labels for the plot
outcomes <- outcomes[with(outcomes, order(event, cases)),] 
  
event_names <- c(" any DUCA complication", "cardiac complication", " every DUCA complication",
                   "30 day mortality", "anastomotic leakage", "prolonged hospital stay (>21 days)", "pulmonal complication")
outcomes$boxLabels <-  rep(event_names, times = 1, each = 5)
  
#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#de6b35", "#008fd5", "#BFBFBF")
barCOLS = c("#008fd5","#de6b35", "#a6d8f0", "#f9b282", "#88E55C")
  
title_name <- "Odds of patients with esophagogastric cancer receiving resection"
subtitle_name <- "based on the number of events in previous cases with EC and GC"
  
  
plot_EC_and_GC <- ggplot(outcomes, aes(x=OR, y=boxLabels, xmin=lower_CI, xmax=upper_CI, col=cases, fill=cases)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed', color = "darkgrey") +
    geom_errorbarh(aes(x = upper_CI, xmin = lower_CI), size = .5, height =
                     .2, color = 'gray50', position=position_dodge(width = 0.6)) +
    geom_text(aes(x = upper_CI, label = p_signif, group = cases), position=position_dodge(width = 0.6), hjust = -1, color = "black") +
    geom_point(size=2, shape=21, colour="black", stroke = 0.5,position=position_dodge(width = 0.6)) +
    scale_fill_manual(values=barCOLS)+
    scale_color_manual(values=dotCOLS)+
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_cartesian(xlim = c(0.4, 2.0)) +
    scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.5, 2), labels = c("0.4", "0.6", "0.8", "1", "1.5", "2")) +
    ylab('') +
    xlab('Odds ratio (log scale)') +
    ggtitle(label = title_name, subtitle = subtitle_name) +
    guides(fill = guide_legend(reverse = TRUE))  +
    theme(legend.title=element_blank()) 
  
plot_EC_and_GC


#load EC results ____________________________________________________________________________________________________________
folder <- paste(main_folder, "/resultaten/final/cases", sep = "")     
load(paste(folder, "/oddsratios GC and EC events in EC patients.Rdata", sep = ""))

outcomes <- outcome_model

outcomes$cases    <- stringr::str_extract(outcomes$event, "(\\d)+(?=cases)")
outcomes$cases    <- paste(outcomes$cases, "cases", sep = "")
outcomes$events   <- sapply(stringr::str_split(outcomes$event, pattern = outcomes$cases), `[`, 1)
outcomes$events   <- sub("ratio_", "", outcomes$events)
outcomes$p_signif <- ifelse(outcomes$p_value < 0.001, '**', ifelse(outcomes$p_value < 0.01, '*', ''))
outcomes$cases    <- sub("([a-z])", " \\1", outcomes$cases)
outcomes$cases    <- factor(outcomes$cases, levels = c("40 cases", "30 cases", "20 cases", "10 cases", "5 cases") )


#order dataframe to match the labels for the plot
outcomes <- outcomes[with(outcomes, order(event, cases)),] 

event_names <- c(" any DUCA complication", "cardiac complication", " every DUCA complication",
                 "30 day mortality", "anastomotic leakage", "prolonged hospital stay (>21 days)", "pulmonal complication")
outcomes$boxLabels <-  rep(event_names, times = 1, each = 5)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#de6b35", "#008fd5", "#BFBFBF")
barCOLS = c("#008fd5","#de6b35", "#a6d8f0", "#f9b282", "#88E55C")


plot_EC <- ggplot(outcomes, aes(x=OR, y=boxLabels, xmin=lower_CI, xmax=upper_CI, col=cases, fill=cases)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed', color = "darkgrey") +
  geom_errorbarh(aes(x = upper_CI, xmin = lower_CI), size = .5, height =
                   .2, color = 'gray50', position=position_dodge(width = 0.6)) +
  geom_text(aes(x = upper_CI, label = p_signif, group = cases), position=position_dodge(width = 0.6), hjust = -1, color = "black") +
  geom_point(size=2, shape=21, colour="black", stroke = 0.5,position=position_dodge(width = 0.6)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0.4, 2.0)) +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.5, 2), labels = c("0.4", "0.6", "0.8", "1", "1.5", "2")) +
  ylab('') +
  xlab('Odds ratio (log scale)') +
  guides(fill = guide_legend(reverse = TRUE))  +
  theme(legend.title=element_blank()) 

plot_EC


#load GC results_________________________________________________________________________________________________________

folder <- paste(main_folder, "/resultaten/final/cases", sep = "")     
load(paste(folder, "/oddsratios GC and EC events in GC patients.Rdata", sep = ""))

outcomes <- outcome_model

outcomes$cases    <- stringr::str_extract(outcomes$event, "(\\d)+(?=cases)")
outcomes$cases    <- paste(outcomes$cases, "cases", sep = "")
outcomes$events   <- sapply(stringr::str_split(outcomes$event, pattern = outcomes$cases), `[`, 1)
outcomes$events   <- sub("ratio_", "", outcomes$events)
outcomes$p_signif <- ifelse(outcomes$p_value < 0.001, '**', ifelse(outcomes$p_value < 0.01, '*', ''))
outcomes$cases    <- sub("([a-z])", " \\1", outcomes$cases)
outcomes$cases    <- factor(outcomes$cases, levels = c("40 cases", "30 cases", "20 cases", "10 cases", "5 cases") )


#order dataframe to match the labels for the plot
outcomes <- outcomes[with(outcomes, order(event, cases)),] 

event_names <- c(" any DUCA complication", "cardiac complication", " every DUCA complication",
                 "30 day mortality", "anastomotic leakage", "prolonged hospital stay (>21 days)", "pulmonal complication")
outcomes$boxLabels <-  rep(event_names, times = 1, each = 5)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#de6b35", "#008fd5", "#BFBFBF")
barCOLS = c("#008fd5","#de6b35", "#a6d8f0", "#f9b282", "#88E55C")


plot_GC <- ggplot(outcomes, aes(x=OR, y=boxLabels, xmin=lower_CI, xmax=upper_CI, col=cases, fill=cases)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed', color = "darkgrey") +
  geom_errorbarh(aes(x = upper_CI, xmin = lower_CI), size = .5, height =
                   .2, color = 'gray50', position=position_dodge(width = 0.6)) +
  geom_text(aes(x = upper_CI, label = p_signif, group = cases), position=position_dodge(width = 0.6), hjust = -1, color = "black") +
  geom_point(size=2, shape=21, colour="black", stroke = 0.5,position=position_dodge(width = 0.6)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0.4, 2.0)) +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.5, 2), labels = c("0.4", "0.6", "0.8", "1", "1.5", "2")) +
  ylab('') +
  xlab('Odds ratio (log scale)') +
  guides(fill = guide_legend(reverse = TRUE))  +
  theme(legend.title=element_blank()) 

plot_GC

# combine plots __________________________________________________________________________________________________________

combined_plot <- cowplot::plot_grid(plot_EC_and_GC, plot_EC, plot_GC, nrow = 3, align = "v", labels = "AUTO")
combined_plot

folder <- (paste(main_folder, "resultaten/final/cases/plots", sep = "/"))     
ggsave("grouped plot.png", plot = combined_plot, path = folder, width = 8, height = 10)





#SUPPLEMENTARY PLOTS

#load EC results ____________________________________________________________________________________________________________
folder <- paste(main_folder, "/resultaten/final/cases", sep = "")     
load(paste(folder, "/oddsratios EC only events in EC patients.Rdata", sep = ""))

outcomes <- outcome_model

outcomes$cases    <- stringr::str_extract(outcomes$event, "(\\d)+(?=cases)")
outcomes$cases    <- paste(outcomes$cases, "cases", sep = "")
outcomes$events   <- sapply(stringr::str_split(outcomes$event, pattern = outcomes$cases), `[`, 1)
outcomes$events   <- sub("ratio_", "", outcomes$events)
outcomes$p_signif <- ifelse(outcomes$p_value < 0.001, '**', ifelse(outcomes$p_value < 0.01, '*', ''))
outcomes$cases    <- sub("([a-z])", " \\1", outcomes$cases)
outcomes$cases    <- factor(outcomes$cases, levels = c("40 cases", "30 cases", "20 cases", "10 cases", "5 cases") )


#order dataframe to match the labels for the plot
outcomes <- outcomes[with(outcomes, order(event, cases)),] 

event_names <- c(" any DUCA complication", "cardiac complication", " every DUCA complication",
                 "30 day mortality", "anastomotic leakage", "prolonged hospital stay (>21 days)", "pulmonal complication")
outcomes$boxLabels <-  rep(event_names, times = 1, each = 5)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#de6b35", "#008fd5", "#BFBFBF")
barCOLS = c("#008fd5","#de6b35", "#a6d8f0", "#f9b282", "#88E55C")

title_name <- "Odds of patients with esophageal cancer receiving resection"
subtitle_name <- "based on the number of events in previous cases with EC"

plot_EC_sup <- ggplot(outcomes, aes(x=OR, y=boxLabels, xmin=lower_CI, xmax=upper_CI, col=cases, fill=cases)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed', color = "darkgrey") +
  geom_errorbarh(aes(x = upper_CI, xmin = lower_CI), size = .5, height =
                   .2, color = 'gray50', position=position_dodge(width = 0.6)) +
  geom_text(aes(x = upper_CI, label = p_signif, group = cases), position=position_dodge(width = 0.6), hjust = -1, color = "black") +
  geom_point(size=2, shape=21, colour="black", stroke = 0.5,position=position_dodge(width = 0.6)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0.4, 3.0)) +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.5, 2, 3), labels = c("0.4", "0.6", "0.8", "1", "1.5", "2", "3")) +
  ylab('') +
  xlab('Odds ratio (log scale)') +
  ggtitle(label = title_name, subtitle = subtitle_name) +
  guides(fill = guide_legend(reverse = TRUE))  +
  theme(legend.title=element_blank()) 

plot_EC_sup

folder <- paste(main_folder, "/resultaten/final/cases/plots", sep = "")  
ggsave("supplementary plot EC.png", plot = plot_EC_sup, path = folder, width = 8, height = 4)


#load GC results_________________________________________________________________________________________________________

folder <- paste(main_folder, "/resultaten/final/cases", sep = "")     
load(paste(folder, "/oddsratios GC only events in GC patients.Rdata", sep = ""))

outcomes <- outcome_model

outcomes$cases    <- stringr::str_extract(outcomes$event, "(\\d)+(?=cases)")
outcomes$cases    <- paste(outcomes$cases, "cases", sep = "")
outcomes$events   <- sapply(stringr::str_split(outcomes$event, pattern = outcomes$cases), `[`, 1)
outcomes$events   <- sub("ratio_", "", outcomes$events)
outcomes$p_signif <- ifelse(outcomes$p_value < 0.001, '**', ifelse(outcomes$p_value < 0.01, '*', ''))
outcomes$cases    <- sub("([a-z])", " \\1", outcomes$cases)
outcomes$cases    <- factor(outcomes$cases, levels = c("40 cases", "30 cases", "20 cases", "10 cases", "5 cases") )


#order dataframe to match the labels for the plot
outcomes <- outcomes[with(outcomes, order(event, cases)),] 

event_names <- c(" any DUCA complication", "cardiac complication", " every DUCA complication",
                 "30 day mortality", "anastomotic leakage", "prolonged hospital stay (>21 days)", "pulmonal complication")
outcomes$boxLabels <-  rep(event_names, times = 1, each = 5)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282", "#de6b35", "#008fd5", "#BFBFBF")
barCOLS = c("#008fd5","#de6b35", "#a6d8f0", "#f9b282", "#88E55C")

title_name <- "Odds of patients with gastric cancer receiving resection"
subtitle_name <- "based on the number of events in previous cases with GC"

plot_GC_sup <- ggplot(outcomes, aes(x=OR, y=boxLabels, xmin=lower_CI, xmax=upper_CI, col=cases, fill=cases)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed', color = "darkgrey") +
  geom_errorbarh(aes(x = upper_CI, xmin = lower_CI), size = .5, height =
                   .2, color = 'gray50', position=position_dodge(width = 0.6)) +
  geom_text(aes(x = upper_CI, label = p_signif, group = cases), position=position_dodge(width = 0.6), hjust = -1, color = "black") +
  geom_point(size=2, shape=21, colour="black", stroke = 0.5,position=position_dodge(width = 0.6)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0.4, 3.0)) +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.5, 2, 3), labels = c("0.4", "0.6", "0.8", "1", "1.5", "2", "3")) +
  ylab('') +
  xlab('Odds ratio (log scale)') +
  ggtitle(label = title_name, subtitle = subtitle_name) +
  guides(fill = guide_legend(reverse = TRUE))  +
  theme(legend.title=element_blank()) 

plot_GC_sup
folder <- paste(main_folder, "/resultaten/final/cases/plots", sep = "")  
ggsave("supplementary plot GC.png", plot = plot_GC_sup, path = folder, width = 8, height = 4)









