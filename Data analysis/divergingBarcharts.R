source("Statistical analysis/data.R")
library(HH)
select <- dplyr::select

# Proportion of a specific value in a column
props.compute <- function(d, int) {
  return(d %>%
           apply(
             MARGIN = 2,
             FUN = function(col) {
               return(length(col[col == int]) / length(col) * 100)
             }
           ))
}

# Proportions of all values between start and end in a column
props.compute.all <- function(d, start, end) {
  props <- data.frame(props.compute(d, start))
  for (i in (start + 1):end) {
    vector <- props.compute(d, i)
    props <- cbind(props, vector)
  }
  names(props) <- paste0(rep("value", length(start:end)), start:end)
  return(props)
}

get_constructs <- function(columns) {
  construct <- c()
  for (col in columns) {
    for (c in constructs) {
      if (col %in% colnames(c[[2]])) {
        construct <- c(construct, c[[1]])
        break
      }
    }
  }
  construct[construct == "Benevolence"] <- "Benevol."
  construct[construct == "One-Dimensional Trust"] <- "1D"
  construct[construct == "Intention to Return"] <- "Return"
  construct[construct == "Transparency"] <- "Transpar."
  construct[construct == "Preference Elicitation"] <- "Pref. Elicitation"
  return(construct)
}

get_diverging_df <- function(data, group, columns) {
  lookup <- c(
    `Q1. Wiski is like an expert [...] for recommending maths exercises` = "Q1",
    `Q2. Wiski has the expertise (knowledge) to estimate my maths level` = "Q2",
    `Q3. Wiski can estimate my maths level` = "Q3",
    `Q4. Wiski understands the difficulty level of maths exercises well` = "Q4",
    `Q5. Wiski [considers] my maths level when recommending exercises` = "Q5",
    `Q6. Wiski prioritises that I improve in maths` = "Q6",
    `Q7. Wiski recommends exercises so that I improve in maths` = "Q7",
    `Q8. Wiski wants to estimate my maths level well` = "Q8",
    `Q9. Wiski recommends exercises as correctly as possible` = "Q9",
    `Q10. Wiski is honest` = "Q10",
    `Q11. Wiski makes integrous recommendations` = "Q11",
    `Q12. I trust Wiski to recommend me maths exercises` = "Q12",
    `Q13. If I want to solve maths exercises again, I will choose Wiski` = "Q13",
    `Q14. If I want recommended maths exercises again, I will choose Wiski` = "Q14",
    `Q15. I understood why the exercises were recommended to me` = "Q15",
    `Q16. Wiski helps me understand [why recommendations were suitable]` = "Q16",
    `Q17. Wiski explains why the exercises are recommended to me` = "Q17",
    `Q18. I feel in control of telling Wiski what I want` = "Q18",
    `Q19. I don’t feel in control of telling Wiski what I want` = "Q19",
    `Q20. I don’t feel in control of specifying and changing my preferences` = "Q20",
    `Q21. Wiski seems to control my decision process rather than me` = "Q21",
    `Q22. Wiski [allows] to express my preferences` = "Q22",
    `Q23. I found it easy to tell Wiski about my preferences` = "Q23",
    `Q24. It is easy to learn to tell Wiski what I like` = "Q24",
    `Q25. It required too much effort to tell Wiski what I like` = "Q25",
    `Q26. Wiski [allows] to revise my preferences` = "Q26",
    `Q27. I found it easy to make Wiski recommend different things to me` = "Q27",
    `Q28. It is easy to train Wiski to update my preferences` = "Q28",
    `Q29. I found it easy to alter the recommended exercises [...]` = "Q29",
    `Q30. It is easy [...] to inform Wiski if I (dis)like recommended exercises` = "Q30",
    `Q31. It is easy for me to get a new set of recommended exercises` = "Q31"
  )
  data_filtered <- data %>%
    filter(Group == group) %>%
    select(one_of(columns)) %>%
    rename(any_of(lookup))
  df <- props.compute.all(data_filtered, 1, 7) %>%
    rename(
      "Completely disagree" = value1,
      "Disagree" = value2,
      "Rather disagree" = value3,
      "Neutral" = value4,
      "Rather agree" = value5,
      "Agree" = value6,
      "Completely agree" = value7
    )
  df$Construct <- get_constructs(columns)
  df$Question <- rownames(df)
  return(df)
}

# Based on https://xang1234.github.io/likert/
make_diverging_barchart <- function(data, group, columns, title) {
  df <- get_diverging_df(data, group, columns)
  lik <- likert(
    Question ~ . | Construct,
    df,
    main = title,
    xlim = c(-75:100),
    xlab = "Percent of responses",
    ylab = NULL,
    layout=c(1,length(unique(df$Construct))),
    scales=list(y=list(relation="free"),
                x=list(
                  at=seq(-60, 100, 20),
                  labels=c(seq(60, 0, -20), seq(20, 100, 20))
                  )
                ),
    between=list(y=0.75),
    par.settings = list(par.main.text=list(x=unit(.8,"npc"))),
    strip.left=strip.custom(bg="gray97"),
    strip=FALSE,
    par.strip.text=list(cex=0.75, lines=1.5),
  )
  plot(lik)
}

png("Images/divergingNone.png", res=300, width = 2200, height = 3000)
make_diverging_barchart(get_data(), 1, paste0(rep("Q",31),c(1:31)), "NONE")
dev.off()
png("Images/divergingControl.png", res=300, width = 2200, height = 3000)
make_diverging_barchart(get_data(), 2, paste0(rep("Q",31),c(1:31)), "CONTROL")
dev.off()
png("Images/divergingImpact.png", res=300, width = 2200, height = 3000)
make_diverging_barchart(get_data(), 3, paste0(rep("Q",31),c(1:31)), "CONTROL+IMPACT")
dev.off()
png("Images/divergingLegend.png", res=300, width = 4500, height = 3000)
make_diverging_barchart(get_data(), 3, paste0(rep("Q",31),c(1:31)), "CONTROL+IMPACT")
dev.off()
