################################################################################
# Project: Anticipatory saccades for prevention actions
# - analysis of error rates


#---------------------------------------------------------
# SET ENVIRONMENT
rm(list=ls()) 
options(digits = 7)
library(tidyverse)


#---------------------------------------------------------
# SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[1]

if (exp == "exp_1"){
  dat <- data.table::fread(".\\Data\\Exp1\\dat_sacc.csv", sep = ",", dec = ".")
} else if (exp == "exp_2") {
  dat <- data.table::fread(".\\Data\\Exp2\\dat_sacc.csv", sep = ",", dec = ".")
}


#---------------------------------------------------------
# AGGREGATE

dat_aggr <- dat %>% 
  # compute whether trial is correct
  mutate(
    correct_trial = ifelse(ActPass == "active" & response == 1 | ActPass == "passive" & response == 0, 1, 0)
  ) %>% 
  filter(ActPass == "active") %>%
  ungroup() %>% 
  group_by(vp, trial, OpPrev, ActPass, correct_trial) %>% 
  count() %>%    # reduce to one data point per trial
  ungroup() %>% 
  group_by(vp, OpPrev, ActPass) %>% 
  summarize(
    mean_correct_trial = mean(correct_trial)
  ) %>% 
  mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)


#---------------------------------------------------------
# STATISTICS

t.test(mean_correct_trial ~ OpPrev, data= dat_aggr, paired = T) %>% schoRsch::t_out()
dat_aggr %>% group_by(OpPrev) %>% summarize(m = mean(mean_correct_trial))



