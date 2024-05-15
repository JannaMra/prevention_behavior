################################################################################
# Project: Anticipatory saccades for prevention actions
# - analysis of picture rating


#---------------------------------------------------------
# SET ENVIRONMENT
rm(list=ls()) 
options(digits = 7)
library(tidyverse)


#---------------------------------------------------------
# SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[2]

if (exp == "exp_1"){
  dat <- data.table::fread(".\\Data\\Exp1\\dat_rating.csv", sep = ",", dec = ".")
} else if (exp == "exp_2") {
  dat <- data.table::fread(".\\Data\\Exp2\\dat_rating.csv", sep = ",", dec = ".")
}


#---------------------------------------------------------
# AGGREGATE

dat_aggr <- dat %>% ungroup() %>% group_by(vp, posneg) %>% summarize(
  mean_valence = mean(valence),
  mean_arousal = mean(arousal)
)

if (exp == "exp_1") {
  t.test(mean_valence ~ posneg, data= dat_aggr, paired = T) %>% schoRsch::t_out()
  dat_aggr %>% group_by(posneg) %>% summarize(m_val = mean(mean_valence)) %>% print()
  t.test(mean_arousal ~ posneg, data= dat_aggr, paired = T) %>% schoRsch::t_out()
  dat_aggr %>% group_by(posneg) %>% summarize(m_aro = mean(mean_arousal)) %>% print()
} else {
  dat_aggr %>% group_by(posneg) %>% summarize(m_val = mean(mean_valence)) %>% print()
  dat_aggr %>% group_by(posneg) %>% summarize(m_aro = mean(mean_arousal)) %>% print()
}


