rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat <- data.table::fread("dat.csv", sep = ",", dec = ".")

dat <- dat %>% ungroup() %>% group_by(ActPass, response) %>% mutate(
  correct_trial = ifelse((ActPass == "active" & response == 1) | (ActPass == "passive" & response == 0), 1, 0)
)

dat <- dat %>% mutate(
  relsacc = ifelse((timest >= time - 600) & (timest <= time), 1, 0),
  saccdur = timeend - timest
)

#------
# exclusion due to < 10 observations per cell
# exclusion Fehler und Baseline ok
dat <- dat %>% filter(vp != "vp21" & vp != "vp22" & vp != "vp35" & vp != "vp37" & vp != "vp49")
dat <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% filter(correct_trial == 1 & blok == 1) 


# exclusion, wenn Sakkade zu lang dauert
#dat <- dat %>% filter(saccdur < 200)



dat <- dat %>% mutate(
   direction = ifelse(yst >= yend, 1, 0)
)

#dat <- dat %>% ungroup() %>% group_by(vp, trial) %>% filter(relsacc == 1) %>% filter(row_number() == 1)
dat <- dat %>% ungroup() %>% group_by(vp, trial) %>% filter(relsacc == 1) %>% filter(direction == 1) %>% filter(row_number() == 1)

dat <- dat %>% ungroup() %>% mutate(
  x_st = xst-blmeanx,
  y_st = yst-blmeany,
  x_end = xend -blmeanx,
  y_end = yend -blmeany
)



dat <- dat %>% mutate(
  saccade_length = y_st - y_end
) %>% filter(!is.na(saccade_length))

# Explorative Bildchen, zum besseren Verständnis, was abgeht --

dat %>% 
  ggplot(aes(x = 1, y = saccade_length, color = OpPrev, shape = ActPass)) + 
  geom_jitter(size = 1, alpha = 1) +
  #scale_y_continuous(limits = c(-1500, -10)) +
  theme_classic()

hist(dat$saccade_length)

dat <- dat %>% mutate(
  fourgroup = ifelse(OpPrev == "operant" & ActPass == "active", "OpAct", NA)
) %>% mutate(
  fourgroup = ifelse(OpPrev == "operant" & ActPass == "passive", "OpPas", fourgroup)
)  %>% mutate(
  fourgroup = ifelse(OpPrev == "prevention" & ActPass == "active", "PrevAct", fourgroup)
) %>% mutate(
  fourgroup = ifelse(OpPrev == "prevention" & ActPass == "passive", "PrevPass", fourgroup)
)

dat %>%
  ggplot(aes(x = saccade_length, color = fourgroup)) +
  geom_histogram(binwidth = 25) +
  #scale_y_continuous(limits = c(-10, 50)) +
  facet_wrap(~fourgroup) +
  theme_classic()

# --


dat_aggr <- dat %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
  mean_saclen  = mean(saccade_length),
  sd_saclen  = sd(saccade_length)
)

dat_aggr <- dat_aggr %>% filter(vp != "vp05" & vp != "vp08" & vp != "vp25" & vp != "vp56")

dat_aggr %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
  mean_saclen  = mean(mean_saclen)
)


dat_aggr <- dat_aggr %>% mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)

ez::ezANOVA(data = dat_aggr, dv = mean_saclen, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()

# Fuer SE_PDs --
dat_saclen_d <- dat_aggr %>%  select(-sd_saclen) %>%
  pivot_wider(names_from = c("ActPass"), values_from = "mean_saclen") %>%
  mutate(
    d_relsacc = active - passive
  ) %>% select(vp, OpPrev, d_relsacc)

dat_saclen_join <- dat_aggr %>% full_join(dat_saclen_d) %>%
  group_by(OpPrev, ActPass) %>% summarize(
    m_relsacc = mean(mean_saclen),
    se_relsacc = sd(d_relsacc) /sqrt(n())
  )
# --

ggplot(data = dat_saclen_join, mapping = aes(x = OpPrev, y = m_relsacc, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = m_relsacc - se_relsacc, ymax = m_relsacc + se_relsacc), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Mean length of first saccade +- SE_PD") +
  theme_classic()



t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(mean_saclen ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevention" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
