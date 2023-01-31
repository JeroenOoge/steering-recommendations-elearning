source("Statistical analysis/data.R")
source("Statistical analysis/boxplots.R")
library(stringr)
library(mirt)
library(xtable)

make_summary <- function(construct) {
  group_by(construct, Group) %>%
    summarise(
      count = n(),
      mean = mean(Average, na.rm = TRUE),
      sd = sd(Average, na.rm = TRUE),
    )
}

# Normal distribution?
check_normality <- function(data) {
  n1 <- with(data, shapiro.test(Average[Group == 1]))
  n2 <- with(data, shapiro.test(Average[Group == 2]))
  n3 <- with(data, shapiro.test(Average[Group == 3]))
  c(n1$p.value, n2$p.value, n3$p.value) > 0.05
  qqplot(data$Average)
}

# Identical variances?
check_variances <- function(data) {
  d12 <- data %>% filter(Group %in% c(1,2))
  d23 <- data %>% filter(Group %in% c(2,3))
  d13 <- data %>% filter(Group %in% c(1,3))
  v1 <- var.test(Average ~ Group, data = d12)
  v2 <- var.test(Average ~ Group, data = d23)
  v3 <- var.test(Average ~ Group, data = d13)
  c(v1$p.value, v2$p.value, v3$p.value) > 0.05
}

for (c in constructs) {
  print(c[[1]])
  data <- c[[2]]
  print(make_summary(data))
  print(check_normality(data))
  print(check_variances(data))
}

# ANOVA
compare_groups_anova <- function(data=constructs) {
  for (c in data) {
    print(c[[1]])
    print(summary(aov(Average ~ Group, data=c[[2]])))
  }
}

compare_groups_anova()

# t-test
#var.equal: a logical variable indicating whether to treat the two variances as 
#being equal. If TRUE then the pooled variance is used to estimate the variance 
#otherwise the Welch test is used.

compare_groups <- function(a, b, data=constructs) {
  names <- c()
  pvalues <- c()
  diffs <- c()
  for (c in data) {
    name <- c[[1]]
    names <- c(names, name)
    data <- c[[2]] %>% filter(Group %in% c(a,b))
    if (name == "One-Dimensional Trust") {
      t <- wilcox.test(Average ~ Group, data = data, paired = FALSE, exact = FALSE, alternative = "less")
      diffs <- c(diffs, round(t$statistic, digits = 2))
    } else {
      t <- t.test(Average ~ Group, data = data, var.equal = TRUE, alternative = "less")
      diffs <- c(diffs, round(t$estimate[2]-t$estimate[1], digits = 2))
    }
    pvalues <- c(pvalues, round(t$p.value, digits = 3))
  }
  data.frame(Construct=names, "pvalue"=pvalues, "diff"=diffs)
}

g12 <- compare_groups(1,2)
g13 <- compare_groups(1,3)
g23 <- compare_groups(2,3)

get_table_df <- function(data) {
  get_diff <- function(x) {
    ifelse(x > 0, paste0("\\phantom{-}", format(x, nsmall=2, trim=TRUE)), format(x, nsmall=2, trim=TRUE))
  }
  get_p <- function(x) {
    stars <- ifelse(x < 0.001, "**",
                    ifelse(x < 0.01, "*", ""))
    paste0("\\scriptsize{($p=", format(x, nsmall=3), "$)}", stars)
  }
  get_text <- function(x,y) {
    ifelse(y < 0.05, paste(get_diff(x), get_p(y)), paste0("\\insignif{", get_diff(x), " ", get_p(y), "}"))
  }
  text <- get_text(data$diff, data$pvalue)
  tibble(Construct=data$Construct, Text=text)
}

get_table_df(g12) %>%
  merge(get_table_df(g13), by="Construct", sort=FALSE) %>%
  merge(get_table_df(g23), by="Construct", sort=FALSE) %>%
  xtable(.) %>%
  print(
    file="Statistical analysis/tests.tex",
    include.rownames = FALSE,
    only.contents = TRUE,
    sanitize.text.function = function(x){x}
  )

g1 <- one_d_trust %>% filter(Group ==1) %>% select(Average) %>% pull() %>% mean()
g2 <- one_d_trust %>% filter(Group ==2) %>% select(Average) %>% pull() %>% mean()
g3 <- one_d_trust %>% filter(Group ==3) %>% select(Average) %>% pull() %>% mean()
round(g2-g1,digits=3)
round(g3-g1,digits=3)
round(g3-g2,digits=3)
