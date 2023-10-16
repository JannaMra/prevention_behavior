rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat <- data.table::fread("dat.csv", sep = ",", dec = ".")


# Berechnung interessantes Zeitintervall
# Alles was innerhalb der 600ms startet

dat <- dat %>% ungroup() %>% group_by(ActPass, response) %>% mutate(
  correct_trial = ifelse((ActPass == "active" & response == 1) | (ActPass == "passive" & response == 0), 1, 0)
)

dat <- dat %>% mutate(
  relsacc = ifelse((timest >= time - 600) & (timest <= time), 1, 0),
  saccdur = timeend - timest
)

#------
# exclusion Fehler und Baseline ok
dat <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% filter(correct_trial == 1 & blok == 1) 


dat_s <- dat

# Alles ausschließen, was weniger als 1 Grad Sehwinkel nach oben geht
# Hier kann auch Kriterium angepasst werden
dat_s <- dat_s %>% mutate(
  direction = ifelse(yst >= yend + 40, 1, 0)
)


dat_s <- dat_s %>% mutate(
  NAincluded = ifelse((is.na(xst) == T | is.na(yst) == T | is.na(xend) == T | is.na(yend) == T), 1, 0)
)


dat_s <- dat_s %>% mutate(
  relsacc_dir = ifelse((relsacc == 1 & direction == 1 & NAincluded == 0), 1, 0)
)


dat_s <- dat_s %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass)

dat_saccnumb <- dat_s %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% summarize(
  sacc_sum = n(),
  relsacc_sum = sum(relsacc_dir)
)


dat_saccnumb <- dat_saccnumb %>% mutate(
  relsacc_any = ifelse(relsacc_sum > 0, 1, 0)
)

dat_saccnumb <- dat_saccnumb %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
   percent = mean(relsacc_any)
 )

# Fuer SE_PDs --
dat_saccnumb_d <- dat_saccnumb %>%  
  pivot_wider(names_from = c("ActPass"), values_from = "percent") %>%
  mutate(
    d_relsacc = active - passive
  ) %>% select(vp, OpPrev, d_relsacc)

dat_sacc_join <- dat_saccnumb %>% full_join(dat_saccnumb_d) %>%
  group_by(OpPrev, ActPass) %>% summarize(
    m_relsacc = mean(percent),
    se_relsacc = sd(d_relsacc) /sqrt(n())
  )
# --


dat_saccnumb <- dat_saccnumb %>% mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)

ez::ezANOVA(data = dat_saccnumb, dv = percent, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()


aggr_saccnumb <- dat_saccnumb %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
  relsacc_mean = mean(percent)
)


ggplot(data = dat_sacc_join, mapping = aes(x = OpPrev, y = m_relsacc, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = m_relsacc - se_relsacc, ymax = m_relsacc + se_relsacc), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Mean number of relevant saccades +- SE_PD") +
  theme_classic()




t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevention" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()


