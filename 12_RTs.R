rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat <- data.table::fread("dat.csv", sep = ",", dec = ".")


dat <- dat %>% ungroup() %>% group_by(ActPass, response) %>% mutate(
  correct_trial = ifelse((ActPass == "active" & response == 1) | (ActPass == "passive" & response == 0), 1, 0)
)

dat_RT <- dat %>% filter(ActPass == "active" & response == 1) %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% summarize(
  rt = mean(rt)
)

dat_RT <- dat_RT %>% ungroup() %>% group_by(vp, OpPrev) %>% summarize(
  rt = mean(rt)
)

aggr_RT <- dat_RT %>% ungroup() %>% group_by(OpPrev) %>% summarize(
  rt = mean(rt)
)

dat_RT_op <- dat_RT %>% filter(OpPrev == "operant")
dat_RT_prev <- dat_RT %>% filter(OpPrev == "prevention")

dat_RT_d <- dat_RT %>%  
  pivot_wider(names_from = c("OpPrev"), values_from = "rt") %>%
  mutate(
    d_RT = operant - prevention
  ) 

dat_RT_join <- dat_RT %>% full_join(dat_RT_d) %>%
  group_by(OpPrev) %>% summarize(
    m_RT = mean(rt),
    se_RT = sd(d_RT) /sqrt(n())
  )

t.test(dat_RT_op$rt, dat_RT_prev$rt, paired = T,  var.equal = T) %>% schoRsch::t_out()

ggplot(data = dat_RT_join, mapping = aes(x = OpPrev, y = m_RT, group = 1)) +
  geom_point(size = 2, color = "#6BBFA3") +
  geom_line(linewidth = 1, color = "#6BBFA3") +
  geom_errorbar(aes(ymin = m_RT - se_RT, ymax = m_RT + se_RT), width = 0.05, color = "#6BBFA3") +
  ylab("Reaction times +- SE_PD") +
  theme_classic()
