library(dplyr)

get_data <- function() {
  return (read.csv("Datasets/data.csv"))
}

make_construct <- function(cols, data=get_data()) {
  construct <- data %>% select(all_of(c("UID", "Group", "Grade", cols)))
  construct$Sum <- rowSums(construct[,cols])
  construct$Average <- construct$Sum/length(cols)
  return(construct)
}

competence <- make_construct(c("Q1", "Q2", "Q3","Q4","Q5"))
benevolence <- make_construct(c("Q6", "Q7", "Q8"))
integrity <- make_construct(c("Q9", "Q10", "Q11"))
one_d_trust<- get_data() %>% select(UID, Group, Grade, Q12) %>% mutate(Average=Q12)
intention_to_return <- make_construct(c("Q13", "Q14"))
transparency <- make_construct(c("Q15", "Q16", "Q17"))
control <- make_construct(c("Q18", "Q19", "Q20", "Q21"))
preference_elicitation <- make_construct(c("Q22", "Q23", "Q24", "Q25"))
preference_revision <- make_construct(c("Q26", "Q27", "Q28", "Q29", "Q30", "Q31"))

trusting_beliefs <- competence %>%
  merge(benevolence, by = "UID") %>%
  merge(integrity, by = "UID") %>%
  rename(Average.z=Average) %>%
  mutate(Average=(Average.x+Average.y+Average.z)/3) %>%
  select(UID, Group, Grade, Average)

multi_d_trust <- trusting_beliefs %>%
  merge(intention_to_return, by = "UID") %>%
  merge(transparency, by = "UID") %>%
  rename(Average.z=Average) %>%
  mutate(Average=(Average.x+Average.y+Average.z)/3) %>%
  select(UID, Group, Grade, Average)

constructs <- list(list("Competence",competence),
                   list("Benevolence",benevolence),
                   list("Integrity",integrity),
                   list("Trusting Beliefs",trusting_beliefs),
                   list("Intention to Return",intention_to_return),
                   list("Transparency",transparency),
                   list("One-Dimensional Trust",one_d_trust),
                   list("Multidimensional Trust", multi_d_trust),
                   list("Control",control),
                   list("Preference Elicitation",preference_elicitation),
                   list("Preference Revision",preference_revision))
