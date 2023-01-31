source("Statistical analysis/data.R")

get_qualitative_data <- function(group) {
  get_data() %>%
  filter(Group == group) %>%
  select(Do.you.trust.Wiski.for.recommending.exercises..Why..not..,
         Do.you.have.more.remarks.to.your.responses...control.,
         Do.you.have.more.remarks.to.your.responses...interaction.,
         Remarks.on.control,
         Remarks.on.impact.visualisation)
}

none <- get_qualitative_data(1)
control <- get_qualitative_data(2)
impact <- get_qualitative_data(3)