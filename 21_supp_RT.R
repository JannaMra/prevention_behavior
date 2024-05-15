################################################################################
# Project: Anticipatory saccades for prevention actions
# - analysis of response times


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
  dat <- data.table::fread(".\\Data\\Exp1\\dat_sacc.csv", sep = ",", dec = ".")
} else if (exp == "exp_2") {
  dat <- data.table::fread(".\\Data\\Exp2\\dat_sacc.csv", sep = ",", dec = ".")
}


#---------------------------------------------------------
# FILTER FOR RELEVANT DATA

# exclude erroneous and passive trials
dat <- dat %>% 
  filter(ActPass == "active" & response == 1)


#---------------------------------------------------------
# AGGREGATE

dat_aggr <- dat %>% 
  ungroup() %>% 
  group_by(vp, trial, OpPrev, rt) %>% 
  count() %>%    # reduce to one data point per trial
  ungroup() %>% 
  group_by(vp, OpPrev) %>% 
  summarize(
    mean_rt = mean(rt)
  ) %>% 
  mutate_at(c("vp", "OpPrev"), as_factor)


#---------------------------------------------------------
# STATISTICS

t.test(mean_rt ~ OpPrev, data= dat_aggr, paired = T) %>% schoRsch::t_out()
dat_aggr %>% group_by(OpPrev) %>% summarize(m = mean(mean_rt))


