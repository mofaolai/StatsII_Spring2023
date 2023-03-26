getwd()

setwd("/Users/marcus/Documents/ASDS/ASAQM II/PS3_MÓF_Answers")

# Importing libraries
library(MASS)
library(nnet)
library(ggplot2)

# Read in the data
gdpChange <- read.csv("gdpChange.csv")


gdpChange$Response[gdpChange$GDPWdiff == 0] <- "No change" 
gdpChange$Response[gdpChange$GDPWdiff > 0] <- "Positive"
gdpChange$Response[gdpChange$GDPWdiff < 0] <- "Negative"

str(gdpChange$Response)

# 1. Unordered multinomial logit with GDPWdiff as the output
# no change as the reference category

gdpChange$Response <- factor(gdpChange$Response,
                             levels = c("No change",
                                        "Positive",
                                        "Negative"),
                             labels = c("No change",
                                        "Positive",
                                        "Negative"))

# a. setting the reference category for the outcome
gdpChange$Response <- relevel(gdpChange$Response,
                              ref = "No change")

# b. running the model
mult.log <- multinom(Response ~ REG + OIL,
                     data = gdpChange)

summary(mult.log)
coef(mult.log)
exp(coef(mult.log))

exp(coef(mult.log)[1])

exp(4.)


# c. Getting the p-values

z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1-pnorm(abs(z), 0, 1))*2)

# d. We can use predicted probabilities to help our 
# interpretation

pp <- data.frame(fitted(mult.log))

head(data.frame(Response= gdpChange$Response,
                NC = pp$No.change,
                P = pp$Positive,
                N = pp$Negative
                ))





# 2. Ordered

# Factoring with ordering, with Negative as the
# lowest ordered category

gdpChange$Response <- factor(gdpChange$Response,
                             levels = c("Negative",
                                        "No change",
                                        "Positive"),
                             labels = c("Negative",
                                        "No change",
                                        "Positive"),
                             ordered = TRUE)

str(gdpChange$Response)

ord.log <- polr(Response ~ REG + OIL,
                data = gdpChange,
                Hess = TRUE)

summary(ord.log)


# Calculating the p value for ordinal regression
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord.log))

# Calculating odds ratio
exp(cbind(OR = coef(ord.log), ci))










#################################################################################

# Question 2

# Q2. a
# Outcome: PAN.visits.06
# Predictor: competitive.district

# Reading in the data
muniData <- read.csv("MexicoMuniData.csv")

# Running Poisson model 1
poisson.reg <- glm(PAN.visits.06 ~ competitive.district,
                   data = muniData,
                   family = poisson)
summary(poisson.reg)



poisson.reg1 <- glm(PAN.visits.06 ~ competitive.district+
                     marginality.06 +
                     PAN.governor.06,
                   data = muniData,
                   family = poisson)
summary(poisson.reg1)


#cfs1 <- coef(poisson.reg)

# Q2.b

cfs <- coef(poisson.reg1)

cfs
exp(cfs[4])

exp(cfs[2])

# Q2.c

# competitive district = 1
# marginality.06 = 0
# PAN.governor.06 = 1

exp(cfs[1] + cfs[2]*0 + cfs[3]*0+ cfs[4]*1)




