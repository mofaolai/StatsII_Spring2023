rm(list = ls())

getwd()
setwd("/Users/marcus/Documents/ASDS/ASAQM II/PS4_MÓF_Answers")

# 1. Load in libraries
library(eha)
library(survival)
library(stargazer)
library(ggplot2)


# 2. Load in data
child <- child

# 3. Creating the survival object.
child_surv <- with(child, Surv(enter, exit, event))


# 4. Run a Cox Proportional Hazard Regression on the 
# data with mother's age and infant's gender as
# covariates. 
cox.reg <- coxph(child_surv ~ m.age + sex,
                 data = child)

drop1(cox.reg,
      test = "Chisq")

stargazer(cox.reg,
          type = "latex")

exp(0.008)
exp(-0.082)

