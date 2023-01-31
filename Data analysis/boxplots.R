library(dplyr)
library(readxl)
library(mirt)
library(ggplot2)
library(ggpubr)
source("Statistical analysis/data.R")

make_boxplot <- function(data,title) {
  ggplot(data, aes(x = factor(Group), y = Average, color=factor(Group))) +
    geom_boxplot( 
      size = 0.5,
      width = 0.6,
      fatten = 1.75,
      outlier.shape = 15) + 
    geom_jitter(size=2, alpha=0.5, width=0.1, height=0.05) +
    labs(
      title=title,
      y = "Score",
      x=element_blank()) + 
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(face="bold", hjust = 0.5, size = 18),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    ylim("1","2","3","4","5","6","7") + 
    scale_x_discrete(labels=c("1" = "NONE", "2" = "CONTROL", "3" = "CONTROL\n+ IMPACT")) +
    scale_color_manual(values = c("#377eb8", "#ff7f00", "#4daf4a"))
}

save_boxplot <- function(data, title) {
  box <- make_boxplot(data,title)
  plot(box)
  ggsave(gsub(" |-", "", paste0("Images/boxplot",title,".pdf")), box, width = 1, height = 1.25, scale=4)
}

make_boxplot_grade <- function(data,title) {
  ggplot(data, aes(x = factor(Group), y = Average, color=factor(Grade))) +
    geom_boxplot( 
      size = 0.5,
      width = 0.6,
      fatten = 1.75,
      outlier.shape = 15,
      color = "black") + 
    geom_jitter(size=2.5, alpha=0.5, width=0.1, height = 0.05) +
    labs(
      title=title,
      y = "Score",
      x=element_blank()) + 
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(face="bold", hjust = 0.5),
          axis.ticks.x = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    ylim("1","2","3","4","5","6","7") + 
    scale_x_discrete(labels=c("1" = "NONE", "2" = "CONTROL", "3" = "CONTROL\n+ IMPACT")) +
    scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00"))
}

save_boxplot_grade <- function(data, title) {
  box <- make_boxplot_grade(data,title)
  plot(box)
  ggsave(gsub(" |-", "", paste0("Images/boxplot",title,"Grade.pdf")), box, width = 1, height = 1.25, scale=4)
}

for (c in constructs) {
  save_boxplot(c[[2]], c[[1]])
  save_boxplot_grade(c[[2]], c[[1]])
}
