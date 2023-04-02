# Reducing the Cost of Voting
# Goodman & Stokes
# Replication code

### LOAD PACKAGES ####
setwd("~/Dropbox/Projects/Internet voting")
getwd()

library(foreign)
library(lmtest) 
library(sandwich)
library(xtable) 
library(Matching)
library(ebal)
library(memisc)
library(car) 
library(rbounds) 
library(ggplot2) 
library(plm)
library(sp)
library(maptools) 
library(rgdal) 
library(rgeos)
library(reshape2) 
library(stringr)  
library(RColorBrewer)
library(apsrtable)
library(classInt)
library(dplyr)
library(plyr)
library(stats)
library(Amelia) 
library(interplot)

rm(list=ls()) #to remove all files 


### FUNCTIONS ####

vcovCluster <- function(
  model,
  cluster
)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K )) 
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}

#### READ IN DATA SET #### ####
munis <- read.csv("Goodman_Stokes_internet_voting_long_panel_data.csv", stringsAsFactors=FALSE, na.strings=c("NA", "NULL", "ACC"), header=TRUE, strip.white = TRUE) # automatically turns acclaimed "ACC" into NA
str(munis,  list.len=150)
munis$muni_id <- as.factor(munis$muni_id)
munis$csd_code <- as.factor(munis$csd_code)


head(munis)
summary(munis)
sum(munis$intvoting==1) #173 treated units
mean(munis$turnout, na.rm=TRUE)

#NA
unique(munis$muni_id[which(is.na(munis$turnout))])
count(unique(munis$muni_id[which(is.na(munis$turnout))])) #9

treated <- munis[which(munis$intvoting==1),] #98 (294/3)
control <- munis[which(munis$intvoting==0),] #247


### TABLE 1 #### ####
#table 1 column 1
id_drop <- munis$csd_code[which(is.na(munis$turnout))] #2 unique ones missing turnout data
munis_dropped <- munis[-which(munis$csd_code %in% id_drop),]
munis_dropped <- droplevels(munis_dropped)
length(unique(munis_dropped$csd_code)) #96

## Table 1 column 1
mod1 <- lm(turnout ~ intvoting + as.factor(csd_code) + as.factor(year), data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)

### Table 1, column 2
mod1 <- lm(turnout ~ intvoting + as.factor(muni_id) + as.factor(year) + as.factor(muni_id):year, data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)

### Table 1 column 3
mod1 <- lm(turnout ~ intvoting + log(pop) + log(pop_den) + unemploy + log(median_inc) + p_65_plus + p_uni + log(p_imm) + as.factor(csd_code) + as.factor(year), data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:10,], 3)


### competitiveness comparison - results reported in text
id_drop <- munis$csd_code[which(is.na(munis$compet & munis$turnout))] 
munis_dropped <- munis[-which(munis$csd_code %in% id_drop),]
munis_dropped <- droplevels(munis_dropped)
length(unique(munis_dropped$csd_code)) #86

id_drop <- munis_dropped$csd_code[which(is.na(munis_dropped$turnout))]
munis_dropped <- munis_dropped[-which(munis_dropped$csd_code %in% id_drop),]
munis_dropped <- droplevels(munis_dropped)
length(unique(munis_dropped$csd_code)) #84

mod1 <- lm(turnout ~ compet + as.factor(csd_code) + as.factor(year), data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)

### robustness check - data without any imputation - as reported in footnote 14
id_drop <- munis$csd_code[which(is.na(munis$turnout_noavg))] #9 unique ones missing turnout data
munis_dropped <- munis[-which(munis$csd_code %in% id_drop),]
munis_dropped <- droplevels(munis_dropped)
length(unique(munis_dropped$csd_code)) #89

## basic model
mod1 <- lm(turnout_noavg ~ intvoting + as.factor(csd_code) + as.factor(year), data=munis_dropped) #0.31
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)

### basic model with linear time trends 
mod1 <- lm(turnout_noavg ~ intvoting + as.factor(muni_id) + as.factor(year) + as.factor(muni_id):year, data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)



#### TABLE 2 #### ####

# Table 2 
id_drop <- munis$csd_code[which(is.na(munis$turnout))] #2 unique ones missing turnout data
munis_dropped <- munis[-which(munis$csd_code %in% id_drop),]
munis_dropped <- droplevels(munis_dropped)
length(unique(munis_dropped$csd_code)) #96

##Table 2, column 1
mod1 <- lm(turnout ~ intvoting + VBM + paper_ballots_eliminated + as.factor(csd_code) + as.factor(year), data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code)), 3)

### Table 2, column 2
mod1 <- lm(turnout ~ intvoting + VBM + paper_ballots_eliminated + as.factor(muni_id) + as.factor(year) + as.factor(muni_id):year, data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:5,], 3)

### Table 2 column 3
mod1 <- lm(turnout ~ intvoting + VBM + paper_ballots_eliminated + log(pop) + log(pop_den) + unemploy + log(median_inc) + p_65_plus + p_uni + log(p_imm) + as.factor(csd_code) + as.factor(year), data=munis_dropped)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis_dropped$csd_code))[1:10,], 3)


#### TABLE 3 ####
###READ IN SMALLER DATA SET ####
munis <- read.csv("Goodman_Stokes_internet_voting_short_panel_data.csv", stringsAsFactors=FALSE, na.strings=c("NA", "NULL", "ACC"), header=TRUE, strip.white = TRUE) # automatically turns acclaimed "ACC" into NA
str(munis,  list.len=150)
munis$muni_id <- as.factor(munis$muni_id)
munis$csd_code <- as.factor(munis$csd_code)

# Drop huntsville because it doesn't use internet voting in 2014
munis <- munis[which(munis$muni_name!="Huntsville"),]

# Drop The Archipelago because it doesn't have an election in 2014 and data cannot be averaged
munis <- munis[which(munis$muni_name!="The Archipelago"),]
munis <- droplevels(munis)

#### ELECTORAL RULES - DIFF IN DIFF 2 YEAR ANALYSIS ####
mod1 <- lm(p_int_votes ~ no_reg_barrier + paper_ballots_eliminated + adv_only + log(pop) + log(pop_den) + unemploy + log(median_inc) + p_65_plus + p_uni + log(p_imm) + as.factor(csd_code) + as.factor(year), data=munis)
round(coeftest(mod1, vcov=vcovCluster(mod1, cluster=munis$csd_code))[1:12,], 3)

nrow(munis[which(munis$paper_ballots_eliminated==1),]) #81
nrow(munis[which(munis$no_reg_barrier==1),]) #84
nrow(munis[which(munis$adv_only==1),]) #6
