#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

climateSupport[4] <- as.logical(ifelse(climateSupport$choice == "Supported", 1, 0))


# Looking at the top of the table
head(climateSupport)

# Looking at the variables
table(climateSupport$choice)
table(climateSupport$countries)
table(climateSupport$sanctions)

?glm()

# GLM 1: Choices influenced by countries

logistic1 <- glm(choice ~ countries, climateSupport, family = "binomial")
summary(logistic1)

# GLM 2: Choices influenced by sanctions

logistic2 <- glm(choice ~ sanctions, climateSupport, family = "binomial")
summary(logistic2)

# GLM 3: Choices influenced by both

logistic3 <- glm(choice ~ ., data= climateSupport, family = "binomial")
summary(logistic3)

# GLM 4: Choices influence by two inputs

logistic <- glm(choice ~ sanctions + countries, climateSupport, family = "binomial")
summary(logistic)

# Data pre-processing

# Turning climate support choice from 'support' /
# 'not support' to TRUE or FALSE 

climateSupport$choice <- as.logical(as.numeric(as.factor(climateSupport$choice))-1) 

# Converting from strings into factors
str(climateSupport)
# Strings are already ordinal factors so we don't need
# to convert? 



# 1. (a) Fit an additive model

logistic4 <- glm(choice ~ sanctions + countries, 
                 climateSupport, 
                 family = "binomial")

summary(logistic4)

# 1. (b) Provide the summary output


# 1. (c) Provide the global null hypothesis

# Creating the null model for a full vs. reduced likelihood ratio test
log_null <- glm(choice ~ 1, family = binomial(link = "logit"), data = climateSupport)

anova(log_null, )

# 1. (d) Provide the p-value

# 1. (e) Describe the results

exp(-0.276332)
exp(-0.181086)
exp(0.150207)
exp(0.458452)

# 1. (f) Provide a conclusion
p <- function(sfive, sfifteen, stwenty, ceighty){
  p <- exp(-0.05 - 0.276332*sfive -0.181086*sfifteen + 0.150207*stwenty + 0.458452*ceighty)
  q <- 1- exp(-0.05 - 0.276332*sfive -0.181086*sfifteen + 0.150207*stwenty + 0.458452*ceighty)
  return(p/q)
}

# 2 (a)

predicted_data <- with(climateSupport, expand.grid(countries = unique(countries),
                                               sanctions = unique(sanctions)))

predicted_data <- cbind(predicted_data, predict(logistic4, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))


# 2 (c)
p(0, 0, 0, 1)


# 2. (d)
interaction_log <- glm(choice ~ sanctions*countries,
                       climateSupport,
                       family = "binomial")

summary(interaction_log)


