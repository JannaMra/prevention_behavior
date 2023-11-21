rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat <- data.table::fread("dat.csv", sep = ",", dec = ".")

# Exclusions Experiment 1: 
# exclusion.responses <- c("vp10", "vp11", "vp12", "vp15", "vp17", "vp27")
# eye.invalid.bl <- c("vp12", "vp34", "vp51")

#------
#Berechnung korrekte Trials

dat <- dat %>% ungroup() %>% group_by(ActPass, response) %>% mutate(
  correct_trial = ifelse((ActPass == "active" & response == 1) | (ActPass == "passive" & response == 0), 1, 0)
)

dat_correct <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% summarize(
  correct = mean(correct_trial)
)

dat_correct <- dat_correct %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
  trialges = n(),
  corr_perc = mean(correct)
)

#Fuer SE_PDs --
dat_correct_d <- dat_correct %>%  select(-trialges) %>%
  pivot_wider(names_from = c("ActPass"), values_from = "corr_perc") %>%
  mutate(
    d_corr_perc = active - passive
  ) %>% select(vp, OpPrev, d_corr_perc)

dat_correct_join <- dat_correct %>% full_join(dat_correct_d) %>%
  group_by(OpPrev, ActPass) %>% summarize(
    m_corr = mean(corr_perc),
    se_corr = sd(d_corr_perc) /sqrt(n())
  )
# --

aggr_correct <- dat_correct %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
  trialges = n(),
  cor_perc = mean(corr_perc)
)

ez::ezANOVA(data = dat_correct, dv = corr_perc, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()

ggplot(data = dat_correct_join, mapping = aes(x = OpPrev, y = m_corr, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = m_corr - se_corr, ymax = m_corr + se_corr), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Percentage correct action taken +- SE_PD") +
  theme_classic()


t.test(corr_perc ~ OpPrev, data= (dat_correct %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(corr_perc ~ OpPrev, data= (dat_correct %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(corr_perc ~ ActPass, data= (dat_correct %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
t.test(corr_perc ~ ActPass, data= (dat_correct %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(corr_perc ~ OpPrev, data= (dat_correct %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(corr_perc ~ OpPrev, data= (dat_correct %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevention" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()

