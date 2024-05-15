################################################################################
# Project: Anticipatory saccades for prevention actions
# - analysis of fixation positions


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
  dat <- data.table::fread(".\\Data\\Exp1\\dat_fixa.csv", sep = ",", dec = ".")
} else if (exp == "exp_2") {
  dat <- data.table::fread(".\\Data\\Exp2\\dat_fixa.csv", sep = ",", dec = ".")
}


#---------------------------------------------------------
# FILTER FOR RELEVANT DATA

# exclude trials with invalid baseline
dat <- dat %>% 
  filter(blok == 1)
# exclude trials with erroneous responses
dat <- dat %>% 
  filter(ActPass == "active" & response == 1 | ActPass == "passive" & response == 0)

# prepare the fixations
dat <- dat %>% mutate(
  is_relevant_time = ifelse((timeend >= time - 600) & (timest <= time), 1, 0),   
  has_NAs = ifelse((is.na(x) | is.na(y)), 1, 0),
  is_ok = ifelse((is_relevant_time == 1 & has_NAs == 0), 1, 0)
) %>% 
  filter(is_ok == 1) %>%
  group_by(vp, trial) %>%
  slice_tail(n = 1) %>%  # select only last fixation of time interval
  mutate(
    x = (x - blmeanx),        # apply baseline correction
    y = (y - blmeany) * (-1)  # so positive indicates upward motion
  )



#---------------------------------------------------------
# AGGREGATE

dat_aggr <- dat %>% 
  ungroup() %>% 
  group_by(vp, OpPrev, ActPass) %>% 
  summarize(
    mean_x  = mean(x),
    sd_x  = sd(x),
    mean_y  = mean(y),
    sd_y  = sd(y)
  ) %>% 
  mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)


#---------------------------------------------------------
# STATISTICS

ez::ezANOVA(data = dat_aggr, dv = mean_y, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()
dat_aggr %>% group_by(OpPrev) %>% summarize(m = mean(mean_y) %>% round(0))
dat_aggr %>% group_by(ActPass) %>% summarize(m = mean(mean_y) %>% round(0))

t.test(mean_y ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()
t.test(mean_y ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
dat_aggr %>% filter(OpPrev == "operant")    %>% group_by(ActPass) %>% summarize(m = mean(mean_y) %>% round(0))
dat_aggr %>% filter(OpPrev == "prevention") %>% group_by(ActPass) %>% summarize(m = mean(mean_y) %>% round(0))

# comparing trials with no pictures (resulting from successful prevention or because this was predetermined)
t.test(mean_y ~ OpPrev, data= (dat_aggr %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
dat_aggr %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive")) %>% group_by(ActPass) %>% summarize(m = mean(mean_y) %>% round(3))



#---------------------------------------------------------
# VISUALIZE


# compute SE_PDs
dat_d <- dat_aggr %>%  select(-c(mean_x, sd_x, sd_y)) %>%
  pivot_wider(names_from = c("ActPass"), values_from = "mean_y") %>%
  mutate(
    d_relfixa = active - passive
  ) %>% select(vp, OpPrev, d_relfixa)

dat_join <- dat_aggr %>% full_join(dat_d) %>%
  group_by(OpPrev, ActPass) %>% summarize(
    m_relfixa = mean(mean_y),
    se_relfixa = sd(d_relfixa) /sqrt(n())
  )


panel_B <- dat_join %>% ggplot(aes(x = OpPrev, y = m_relfixa, group = ActPass, color = ActPass, shape = ActPass)) +
  geom_path(linewidth = 1, aes(linetype = ActPass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = m_relfixa - se_relfixa, ymax = m_relfixa + se_relfixa), width = 0.10, show.legend = FALSE) +
  
  scale_color_manual(name = "Controllability", labels = c("active", "passive"), values = c("#880000", "#1947A3")) +
  scale_shape_manual(name = "Controllability", labels = c("active", "passive"), values = c(19, 17)) + 
  scale_linetype(name = "Controllability") +
  
  scale_x_discrete(name = "Action Type") +
  scale_y_continuous(name = expression(bold("Y Position of Last Anticipatory Fixation ± SE"["PD"])), limits = c(25, 225)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"), 
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"), 
    legend.text = element_text(size = 12),
    
    legend.key.height = unit(20, "points"),
    legend.key.width = unit(44, "points")
  )

# panel_B

if (exp == "exp_1"){
  save(panel_B, file = "e1_panel_B.RData")
} else if (exp == "exp_2") {
  save(panel_B, file = "e2_panel_B.RData")
}


