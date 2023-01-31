source("Statistical analysis/data.R")
source("Statistical analysis/boxplots.R")
library(ggplot2)
library(reshape2)
library(ggExtra)
library(ggtext)

# Overall Elo evolution
transactions <- read.csv("Datasets/userpoints_txn.csv") %>%
  filter(uid %in% get_data()$UID) %>%
  select(uid, time_stamp, points, description) %>%
  rename(UID=uid,timestamp=time_stamp) %>%
  merge(get_data() %>% select(UID, Group), by="UID") %>%
  arrange(UID,timestamp)

get_elo_evolutions <- function(data) {
  elos <- c()
  steps <- c()
  for (uid in unique(data$UID)) {
    t <- data %>% filter(UID == uid)
    elos <- c(elos, cumsum(t$points))
    steps <- c(steps, rownames(t))
  }
  data$elo <- elos
  data$step <- as.numeric(steps)
  return(data %>%
           filter(step > 1) %>%
           mutate(step=step-1))
}

elo_evolution <- get_elo_evolutions(transactions)
ggplot(elo_evolution, aes(step, elo, group=UID)) +
  geom_line()

# Distributions of Elo changes
elo_changes <- transactions %>% filter(grepl("question", description))
make_boxplot(elo_changes %>% rename(Average=points), "Elo changes after solving exercise") +
  scale_y_continuous()
ggplot(elo_changes, aes(points, group=Group, fill=factor(Group))) +
  geom_density(alpha=0.5)

attempts <- elo_changes %>%
  group_by(UID) %>%
  summarise(n=n()) %>%
  merge(elo_changes %>% select(UID, Group) %>% distinct())
make_boxplot(attempts %>% rename(Average=n), "Number of attempts") +
  scale_y_continuous()

# Participants who stopped at least one exercise series before the survey
flags <- read.csv("Datasets/logFlagStop.csv") %>%
  filter(uid %in% transactions$UID) %>%
  rename(UID=uid)
flag_groups <- get_data() %>% filter(UID %in% flags$UID) %>% select(UID, Group)
stopped <- flags %>%
  merge(flag_groups, by="UID") %>%
  filter(Group != 1)
survey <- read.csv("Datasets/logVisitDemografie.csv") %>%
  filter(uid %in% stopped$UID) %>%
  rename(UID=uid,timestampSurvey=timestamp)
stopped_before_survey <- stopped %>%
  merge(survey, by="UID") %>%
  filter(timestamp < timestampSurvey)
transactions %>% filter(!UID %in% unique(stopped_before_survey$UID)) %>%
  arrange(UID)

# Elo evolution before survey completion
survey <- read.csv("Datasets/logVisitDemografie.csv") %>%
  filter(uid %in% elo_evolution$UID) %>%
  rename(UID=uid,timestampSurvey=timestamp)
elo_evolution_before_survey <- elo_evolution %>%
  merge(survey) %>%
  filter(timestamp < timestampSurvey)
exercises_before_survey <- elo_evolution_before_survey %>%
  filter(grepl("question",description) & points>=0) %>%
  group_by(UID) %>%
  mutate(n=n()) %>%
  select(UID, n) %>%
  distinct()
ggplot(exercises_before_survey) +
  geom_histogram(aes(n))
elo_evolution_6exercises <- elo_evolution_before_survey %>%
  filter(points>=0 | !grepl("question",description)) %>%
  filter(UID %in% pull(exercises_before_survey %>% filter(n>=6) %>% select(UID)))

make_evolution_first6 <- function(data) {
  total <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("UID","Group","elo","description"))
  for (uid in unique(elo_evolution_6exercises$UID)) {
    dataUser <- elo_evolution_6exercises %>%
      filter(UID == uid)
    if (dataUser[1,]$description != "Inschatting niveau") {
      row <- dataUser[1,]
      row$elo <- 1500
      row$description <- "Inschatting niveau"
      dataUser <- rbind(dataUser, row)
    }
    df <- dataUser%>%
      filter(description == "Inschatting niveau") %>%
      select(UID,Group,elo,description)
    df$description <- c("Start")
    exercises <- dataUser %>% filter(grepl("question", description))
    control <- dataUser %>% filter(description == "Feedback gebruiker")
    for (i in c(2,4,6)) {
      new_rows <- exercises[(i-1):i,] %>% select(UID,Group,elo)
      new_rows$description <- c(paste0("Exercise",i-1), paste0("Exercise",i))
      df <- df %>% rbind(new_rows)
      feedback <- control %>% filter(step == exercises[i,]$step + 1)
      if (nrow(feedback) > 0) {
        feedback <- feedback %>% select(UID,Group,elo)
      } else {
        feedback <- exercises[i,] %>% select(UID,Group,elo)
      }
      feedback$description <- paste0("Control", i/2)
      df <- df %>% rbind(feedback)
    }
    total <- rbind(total,df)
  }
  return(total)
}
elo_evolution_first6 <- make_evolution_first6(elo_evolution_6exercises)

make_linechart_first6 <- function() {
  # colours <- c("#377eb8", "#ff7f00", "#4daf4a")
  # palette <- rep("#cccccc",3)
  # palette[group] <- colours[group]
  ggplot(elo_evolution_first6, aes(description,elo,group=UID)) +
    # geom_line(aes(colour=factor(Group))) +
    geom_line(colour="#cccccc", size=0.25) +
    scale_x_discrete(limits = c("Start","Exercise1","Exercise2","Control1","Exercise3","Exercise4","Control2","Exercise5","Exercise6","Control3")) +
    scale_y_continuous(n.breaks = 11) +
    theme_classic() +
    # scale_color_manual(values=palette) +
    geom_line(aes(description,mean),
              elo_evolution_first6 %>% filter(Group == 1) %>% group_by(description) %>% mutate(mean=mean(elo)),
              colour="#377eb8",
              size=1) +
    geom_line(aes(description,mean),
              elo_evolution_first6 %>% filter(Group == 2) %>% group_by(description) %>% mutate(mean=mean(elo)),
              colour="#ff7f00",
              size=1) +
    geom_line(aes(description,mean),
              elo_evolution_first6 %>% filter(Group == 3) %>% group_by(description) %>% mutate(mean=mean(elo)),
              colour="#4daf4a",
              size=1) +
    theme(
      axis.line = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1),
      plot.title = element_markdown(lineheight = 1.5)
    ) +
    labs(
      x = element_blank(),
      y = element_blank(),
      title = "**Evolution of participants' Elo ratings during the experiment**
      <br>
      <span style='font-size:11pt'>Average trend for participants in
    <span style='color:#377eb8;'>NONE</span>, 
    <span style='color:#ff7f00;'>CONTROL</span>, and
    <span style='color:#4daf4a;'>CONTROL+IMPACT</span>
    </span>"
    )
}
make_linechart_first6()
ggsave("Images/evolutionElo.pdf")

# Relation between elo changes due to solving exercises and due to control
get_elo_at <- function(data, t) {
  return(data %>% filter(description == t) %>% select(elo) %>% as.numeric())
}
get_elo_control <- function(data) {
  uids <- c()
  groups <- c()
  eloExercises <- c()
  eloControl <- c()
  for (uid in unique(data$UID)) {
    dataUser <- data %>% filter(UID==uid)
    uids <- c(uids, rep(uid, 3))
    group <- dataUser %>%
      select(Group) %>%
      unique() %>%
      as.numeric()
    groups <- c(groups, rep(group, 3))
    diff1 <- get_elo_at(dataUser, "Exercise2") - get_elo_at(dataUser, "Start")
    diff2 <- get_elo_at(dataUser, "Exercise4") - get_elo_at(dataUser, "Control1")
    diff3 <- get_elo_at(dataUser, "Exercise6") - get_elo_at(dataUser, "Control2")
    eloExercises <- c(eloExercises,diff1,diff2,diff3)
    cont1 <- (get_elo_at(dataUser, "Control1") - get_elo_at(dataUser, "Exercise2")) / get_elo_at(dataUser, "Exercise2")
    cont2 <- (get_elo_at(dataUser, "Control2") - get_elo_at(dataUser, "Exercise4")) / get_elo_at(dataUser, "Exercise4")
    cont3 <- (get_elo_at(dataUser, "Control3") - get_elo_at(dataUser, "Exercise6")) / get_elo_at(dataUser, "Exercise6")
    eloControl <- c(eloControl, cont1, cont2, cont3)
  }
  return(data_frame(UID=uids, Group=groups, eloExercises=eloExercises, eloControl=eloControl))
}
elo_control <- get_elo_control(elo_evolution_first6 %>%
                                 filter(Group %in% c(2,3)))
ggplot(elo_control, aes(eloExercises, round(eloControl*100,0), colour=factor(Group))) +
  geom_hline(yintercept = 0, color="lightgrey") +
  geom_vline(xintercept = 0, color="lightgrey") +
  geom_point(size=2, alpha=0.5) + 
  geom_rug(colour="lightgrey", size=0.5, alpha=0.5, sides = "b") +
  theme_classic() +
  labs(x="Change in Elo after an exercise series",
       y="Percentage of control") +
  scale_colour_manual(name = "Group",
                      labels=c("1" = "NONE", "2" = "CONTROL", "3" = "CONTROL+IMPACT"),
                      values = c("#ff7f00", "#4daf4a")) +
  scale_x_continuous(limits = c(-225,225)) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.title = element_text(face = "bold"),
    legend.position="top"
  )
ggsave("Images/controlPercentages.pdf")

# Distribution of control
ggplot(elo_control, aes(x = factor(Group), y = eloControl, color=factor(Group))) +
  geom_boxplot( 
    size = 0.5,
    width = 0.6,
    fatten = 1.75,
    outlier.shape = 15) + 
  geom_jitter(size=2, alpha=0.5, width=0.1) +
  labs(
    title="Control",
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
  scale_x_discrete(labels=c("1" = "NONE", "2" = "CONTROL", "3" = "CONTROL\n+ IMPACT")) +
  scale_color_manual(values = c("#377eb8", "#ff7f00", "#4daf4a"))

t.test(eloControl ~ Group, data = elo_control, var.equal = TRUE, alternative = "greater")

# Start Elo values
elo_start <- elo_evolution_first6 %>% filter(description == "Start")
ggplot(elo_start, aes(elo, group=factor(Group), colour=factor(Group))) +
  geom_density(aes(fill=factor(Group),alpha=0.5))
ggplot(elo_start, aes(elo, group=Group, colour=factor(Group))) +
  geom_boxplot()

t.test(elo ~ Group, data = elo_start %>% filter(Group %in% c(1,2)), var.equal = TRUE)
t.test(elo ~ Group, data = elo_start %>% filter(Group %in% c(1,3)), var.equal = TRUE)
t.test(elo ~ Group, data = elo_start %>% filter(Group %in% c(2,3)), var.equal = TRUE)

# Compare start and end Elo
elo_evolution_startend <- get_startend_elo(transactions)
ggplot(elo_evolution_startend, aes(x = step, y=elo, group=UID, colour=factor(Group))) +
  geom_line() +
  scale_color_discrete() +
  scale_x_discrete(limits=rev) +
  theme_classic()

elo_diffs <- elo_evolution_first6 %>%
  reshape(idvar = c("UID", "Group"), v.names="elo", timevar="description", direction = "wide") %>%
  mutate(
    diff=elo.Control3 - elo.Start,
    diffNoControl=elo.Exercise6 - (elo.Control2-elo.Exercise4) - (elo.Control1-elo.Exercise2) - elo.Start
  )
print(t.test(diff ~ Group, data = elo_diffs %>% filter(Group %in% c(1,2)), var.equal = TRUE, alternative = "less"))
print(t.test(diff ~ Group, data = elo_diffs %>% filter(Group %in% c(1,3)), var.equal = TRUE, alternative = "less"))
print(t.test(diff ~ Group, data = elo_diffs %>% filter(Group %in% c(2,3)), var.equal = TRUE, alternative = "two.sided"))
print(t.test(diffNoControl ~ Group, data = elo_diffs %>% filter(Group %in% c(1,2)), var.equal = TRUE, alternative = "less"))
print(t.test(diffNoControl ~ Group, data = elo_diffs %>% filter(Group %in% c(1,3)), var.equal = TRUE, alternative = "less"))
print(t.test(diffNoControl ~ Group, data = elo_diffs %>% filter(Group %in% c(2,3)), var.equal = TRUE, alternative = "two.sided"))

# Number of different exercises
get_number_different_exercises <- function(data, group) {
  data %>% filter(Group == group) %>%
    filter(grepl("question",description)) %>%
    group_by(UID) %>%
    transmute(n=n_distinct(description)) %>%
    unique() %>%
    ungroup() %>%
    select(n) %>%
    table()
}
get_number_different_exercises(elo_evolution_6exercises, 1)
get_number_different_exercises(elo_evolution_6exercises, 2)
get_number_different_exercises(elo_evolution_6exercises, 3)
