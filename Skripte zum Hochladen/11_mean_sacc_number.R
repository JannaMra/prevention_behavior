################################################################################
# Project: Anticipatory saccades for prevention actions
# - analysis of saccade percentages


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
# EXCLUSION DESCRIPTIVES

dat %>% 
  mutate(
    response_ok = (ActPass == "active" & response == 1 | ActPass == "passive" & response == 0)
    ) %>% 
  group_by(vp, trial, blok, response_ok) %>% 
  count() %>%  # reduce to only one row per trial 
  ungroup() %>%
  summarize(
    m_error = round((1-mean(response_ok))*100, 1),
    m_bad_baseline = round((1-mean(blok))*100, 1)
  )



#---------------------------------------------------------
# FILTER FOR RELEVANT DATA

# exclude trials with invalid baseline
dat <- dat %>% 
  filter(blok == 1)
# exclude trials with erroneous responses
dat <- dat %>% 
  filter(ActPass == "active" & response == 1 | ActPass == "passive" & response == 0)

# prepare the saccades
dat <- dat %>% mutate(
  is_relevant_time = ifelse((timest >= time - 600) & (timest <= time), 1, 0),   # starts in interesting time interval?
  is_relevant_direction = ifelse(yst >= yend + 40, 1, 0),                       # goes upwards by at least 40 px (here: 1 degree visual angle)?
  has_NAs = ifelse((is.na(xst) | is.na(yst) | is.na(xend) | is.na(yend)), 1, 0),
  is_ok = ifelse((is_relevant_time == 1 & is_relevant_direction == 1 & has_NAs == 0), 1, 0)
)



#---------------------------------------------------------
# AGGREGATE

dat_saccnumb <- dat %>% 
  ungroup() %>% 
  group_by(vp, trial, OpPrev, ActPass) %>% 
  summarize(
    n_relevant_sacc = sum(is_ok)
  ) %>% 
  mutate(
    has_any_relevant_sacc = ifelse(n_relevant_sacc > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(vp, OpPrev, ActPass) %>% 
  summarize(
    percent = mean(has_any_relevant_sacc),
    n = n()
  ) %>% 
  mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)



#---------------------------------------------------------
# STATISTICS

ez::ezANOVA(data = dat_saccnumb, dv = percent, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()
dat_saccnumb %>% group_by(OpPrev) %>% summarize(m = mean(percent) %>% round(3))
dat_saccnumb %>% group_by(ActPass) %>% summarize(m = mean(percent) %>% round(3))

t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()
t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
dat_saccnumb %>% filter(OpPrev == "operant")    %>% group_by(ActPass) %>% summarize(m = mean(percent) %>% round(3))
dat_saccnumb %>% filter(OpPrev == "prevention") %>% group_by(ActPass) %>% summarize(m = mean(percent) %>% round(3))

# comparing trials with no pictures (resulting from successful prevention or because this was predetermined)
t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
dat_saccnumb %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive")) %>% group_by(ActPass) %>% summarize(m = mean(percent) %>% round(3))



#---------------------------------------------------------
# VISUALIZE

# compute SE_PDs
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


panel_A <- dat_sacc_join %>% ggplot(aes(x = OpPrev, y = m_relsacc, group = ActPass, color = ActPass, shape = ActPass)) +
  geom_path(linewidth = 1, aes(linetype = ActPass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = m_relsacc - se_relsacc, ymax = m_relsacc + se_relsacc), width = 0.10, show.legend = FALSE) +
  
  scale_color_manual(name = "Controllability", labels = c("active", "passive"), values = c("#880000", "#1947A3")) +
  scale_shape_manual(name = "Controllability", labels = c("active", "passive"), values = c(19, 17)) +
  scale_linetype(name = "Controllability") +
  
  scale_x_discrete(name = "Action Type") +
  scale_y_continuous(name = expression(bold("Number of Anticipatory Saccades ± SE"["PD"])), limits = c(0.15, 0.66)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"), 
    legend.text = element_text(size = 12),
    
    legend.key.height = unit(20, "points"),
    legend.key.width = unit(44, "points")
  )

# panel_A

if (exp == "exp_1"){
  save(panel_A, file = "e1_panel_A.RData")
} else if (exp == "exp_2") {
  save(panel_A, file = "e2_panel_A.RData")
}


