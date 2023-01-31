source("Statistical analysis/data.R")
library(ggplot2)
library(ggtext)

# Grades per research group
data <- get_data() %>%
  group_by(Grade, Group) %>%
  transmute(n=n()) %>%
  unique() %>%
  arrange(Grade, Group) %>%
  group_by(Grade) %>%
  mutate(Total=sum(n))

barchart_grade_legend <- ggplot(data, aes(x=n,y=Grade, fill=factor(Group))) +
  geom_bar(position=position_stack(reverse = TRUE), stat = "identity") +
  geom_text(aes(Total + 0.5, Grade, label=Total, group=Grade), hjust=0, size = 3, colour = "grey") +
  theme_bw() +
  labs(
    x = element_blank(),
    y = element_blank(),
  ) +
  theme(
    plot.title = element_text(face="bold", hjust = 0.5, size = 18),
    axis.text.y = element_text(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(size=0.25),
    panel.grid.major.x = element_line(size=0.25),
    legend.text=element_markdown(),
  ) +
  scale_x_continuous(limits = c(0,35)) +
  scale_y_discrete(labels = c("1ste middelbaar" = "7th grade",
                              "2de middelbaar" = "8th grade",
                              "3de middelbaar" = "9th grade",
                              "4de middelbaar" = "10th grade",
                              "5de middelbaar" = "11th grade",
                              "6de middelbaar" = "12th grade")) +
  scale_fill_manual(name = "Research group",
                    values = c("#377eb8", "#ff7f00", "#4daf4a"),
                    labels = c("1" = "NONE <span style='color:#377eb8'>(22)</span>",
                               "2" = "CONTROL <span style='color:#ff7f00'>(25)</span>",
                               "3" = "CONTROL+IMPACT <span style='color:#4daf4a'>(24)</span>"))

plot(barchart_grade_legend)
ggsave(gsub(" |-", "", paste0("Images/barchartGrade.pdf")), barchart_grade_legend, width = 3, height = 1, scale=2)

barchart_grade <- ggplot(data, aes(x=n,y=Grade, fill=factor(Group))) +
  geom_bar(position=position_stack(reverse = TRUE), stat = "identity", width=0.8) +
  geom_text(aes(Total + 0.5, Grade, label=Total, group=Grade), hjust=0, size = 3, colour = "grey") +
  theme_bw() +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "**Participants distributed over the research groups**<br>
    <span style='font-size:11pt'><span style='color:#377eb8'>NONE (22)</span>, 
    <span style='color:#ff7f00'>CONTROL (25)</span>, and 
    <span style='color:#4daf4a'>CONTROL+IMPACT (24)</span></span>",
  ) +
  theme(
    axis.text.y = element_text(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(size=0.25),
    panel.grid.major.x = element_line(size=0.25),
    plot.title = element_markdown(lineheight = 1.5),
    legend.position="none",
  ) +
  scale_x_continuous(limits = c(0,35)) +
  scale_y_discrete(labels = c("1ste middelbaar" = "7th grade",
                              "2de middelbaar" = "8th grade",
                              "3de middelbaar" = "9th grade",
                              "4de middelbaar" = "10th grade",
                              "5de middelbaar" = "11th grade",
                              "6de middelbaar" = "12th grade")) +
  scale_fill_manual(values = c("#377eb8", "#ff7f00", "#4daf4a"))

plot(barchart_grade)
ggsave(gsub(" |-", "", paste0("Images/barchartGrade.pdf")), barchart_grade, width = 3, height = 1.5, scale=2)
