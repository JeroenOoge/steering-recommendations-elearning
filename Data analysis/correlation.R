source("Statistical analysis/data.R")
library(ggplot2)
library(reshape2)
library(GGally)
library(RColorBrewer)

averages <- get_data() %>% select(UID)
for (c in constructs) {
  df <- c[[2]] %>%
    select(UID, Average) %>%
    rename(!!c[[1]] := Average)
  averages <- averages %>% inner_join(df, by = "UID")
}
averages <- averages %>% select(-UID)

# Heatmap
corrmatrix = function(d) {
  data.cor = round(cor(as.matrix(d)), 2) %>% melt()
}

correlogram = function(d) {
  ggplot(data = d, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    coord_equal() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 3.25) +
    theme_classic() +
    scale_x_discrete(name = NULL) +
    scale_y_discrete(name = NULL,
                     limits = rev(levels(d$Var2))) +
    scale_fill_distiller(palette = "Reds", direction = 1, breaks=seq(0,1,0.25)) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_text(angle=90,
                                 hjust = 1,
                                 vjust = 1),
      legend.title = element_blank(),
      legend.justification = "right",
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.height = unit(.1,"npc")
    )
}

corr <- corrmatrix(averages)
corr$value <- round(corr$value,2)

correlogram(corr)

# Grid of scatter plots, density plots, and correlation values
averages_filtered <- averages %>% select(-c(`Multidimensional Trust`,`Trusting Beliefs`))

lowerfun <- function(data, mapping) {
  f <- 1
  bc <- "black"
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- cor.test(x, y)
  if (corr$p.value >= 0.05) {
    f <- 0.1
    bc <- "#bfbfbf"
  }
  ggplot(data = data, mapping = mapping) +
    geom_jitter(alpha = 0.25 * f, width = 0.05, height = 0.05)+
    theme_classic() +
    xlim("1","2","3","4","5","6","7") +
    ylim("1","2","3","4","5","6","7") +
    theme(axis.line = element_blank(),
          panel.border = element_rect(colour = bc, fill=NA, size=1))+
    geom_line(stat="smooth", method="lm", se=FALSE, colour="#377eb8", size=0.75, alpha=f)
}
# lowerfun(averages, aes(Competence, Benevolence))
# lowerfun(averages, aes(Control, Benevolence))

diagfun <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) +
    geom_density(color="#377eb8", fill="#377eb8", alpha=0.25, size=0.75) +
    theme_classic() +
    xlim("1","2","3","4","5","6","7") +
    ylim(0,1) +
    theme(axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
}
# diagfun(averages, aes(Control))

get_corr_text <- function(x,y) {
  corr <- cor.test(x, y)
  p <- corr$p.value
  stars <- ifelse(p < 0.001, "**",
                  ifelse(p < 0.01, "*", ""))
  return(paste0(format(round(corr$estimate, digits=2), nsmall = 2), stars))
}

# Based on https://stackoverflow.com/a/53685979/2625920
upperfun <- function(data, mapping) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- cor.test(x, y)
  palette <- colorRampPalette(brewer.pal(7, "RdBu"), interpolate ='spline')
  fill <- if (corr$p.value < 0.05) {palette(100)[100-findInterval(corr$estimate, seq(-1, 1, length=100))]} else {"white"}
  bc <- "black"
  if (corr$p.value >= 0.05) {bc <- "#bfbfbf"}
  ggally_statistic(data = data, mapping = mapping, title = NULL, title_args = c(colour=bc), sep="", text_fn = get_corr_text) + 
    theme_void() +
    theme(
      panel.background = element_rect(fill=fill),
      panel.border = element_rect(colour = bc, fill=NA, size=1)
    )
}
# upperfun(averages, aes(Competence, Benevolence))
# upperfun(averages, aes(Control, Benevolence))

make_grid <- function() {
  data <- averages_filtered %>%
    rename(
      "Return" = "Intention to Return",
      "1D Trust" = "One-Dimensional Trust",
      "Pref. Elicitation" = "Preference Elicitation",
      "Pref. Revision" = "Preference Revision",
    )
  ggpairs(data,
          lower = list(continuous = wrap(lowerfun)),
          diag = list(continuous = wrap(diagfun)),
          upper = list(continuous = wrap(upperfun)))
}

plot_corr <- make_grid()
ggsave("Images/correlations.pdf", plot_corr, width = 3000, height = 3000, units="px")
