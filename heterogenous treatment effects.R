library(here)
library(ggplot2)
library(dplyr)
library(data.table)
library(networkD3)
library(stargazer)
library(sandwich)
library(lmtest)

df <- fread("../data/207_final_proj_data.csv")

#head(df)
names(df) <- c('dpt', 'grade', 'res', 't1', 't2', 'y')

df <- df %>%
  dplyr::mutate(dpt_A = ifelse(dpt == "A", T, F),
                dpt_B = ifelse(dpt == "B", T, F),
                grade_1 = ifelse(grade == "grade 1", T, F),
                grade_2 = ifelse(grade == "grade 2", T, F),
                t1_3q = ifelse(t1 == "3 questions", T, F),
                t1_5q = ifelse(t1 == "5 questions", T, F),
                t2 = ifelse(t2 == "likert", T, F)) %>%
  select(-c(dpt, grade, t1)) %>%
  dplyr::filter(res > 0)

head(df)

m1 <- lm(y ~ t1_3q + t1_5q + t1_3q*t2 + t1_5q*t2, data = df)
# we check for heterogenity within department and salary grade and interactions therein
# --> no statistically significant results. 
# (mention t1_5qTRUE:grade_1TRUE case. Although this is likely not significant. Use bonferoni explanation) 
m2 <- lm(y ~ (t1_3q + t1_5q + t2 + dpt_A + dpt_B + grade_1 + grade_2 + res)^2, data = df)

m3 <- lm(y ~ (t1_3q + t1_5q + t2)^2 + dpt_A + dpt_B + grade_1 + grade_2 + log(res), data = df)

m4 <- lm(y ~ (t1_3q + t1_5q + t2)^2 + (dpt_A + dpt_B + grade_1 + grade_2 + log(res))^2, data = df)

m1$vcovHC_ <- sandwich::vcovHC(m1)
coeftest(m1, vcov.=m1$vcovHC_)		#find RSE for each parameter

m2$vcovHC_ <- sandwich::vcovHC(m2)
coeftest(m2, vcov.=m2$vcovHC_)		#find RSE for each parameter

m3$vcovHC_ <- sandwich::vcovHC(m3)
coeftest(m3, vcov.=m3$vcovHC_)		#find RSE for each parameter
