## Assignment 1, Nina van Gerwen, 1860852, Computational Inference with R, Utrecht University

setwd("GitHub/Computational-inference-with-R/")

library(foreign)
library(ggplot2)
library(RColorBrewer)

cols <- brewer.pal(11, "RdBu")
cols

therapy_data <- read.spss("therapy_LONG.sav", to.data.frame = TRUE)

mean_per_two_factors <- aggregate(x = therapy_data$anxiety, by = list(therapy_data$cond, therapy_data$time), FUN = mean)
variance_per_two_factors <- aggregate(x = therapy_data$anxiety, by = list(therapy_data$cond, therapy_data$time), FUN = var)

total_data <- cbind(mean_per_two_factors, variance_per_two_factors$x)

## Calculate the standard error for every group on every phase
total_data$se <- sqrt(total_data$`variance_per_two_factors$x`) / sqrt(30)

Profile_Plot <- ggplot(data = total_data, aes(x = Group.2, y = x, group = Group.1, colour = Group.1)) +
  ## create points for estimations for each group
  geom_point(shape = 18, size = 2) +
  ## create a line between all point estimations for each group
  geom_line(linetype = "longdash") + 
  ## create an interval for every point estimation in the graph through geom_errorbar
  geom_errorbar(aes(ymin = x - 1.96*se, ymax = x + 1.96*se), width = .1) +
  ## apply every possible label to the graph, including legend label
  labs(x = "Phase", y = "Anxiety", title = "Figure 1", subtitle = "Anxiety as a function of time for two conditions", color = "Condition") +
  ## inverse the labels for the conditions in the legend
  guides(color = guide_legend(reverse = TRUE)) +
  ## change the colours and labels for the different conditions and apply a general theme
  scale_colour_manual(values = c("#2166AC", "#B2182B"), labels = c("Control", "Group Therapy")) + theme_bw() +
  ## position the legend on the top-right outside of the graph
  theme(legend.justification = "top", plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic")) +
  ## change the labels on the x-axis
  scale_x_discrete(limits = c("pretest", "posttest", "followup"), labels = c("Pretest", "Posttest", "Follow-up"))

Profile_Plot

