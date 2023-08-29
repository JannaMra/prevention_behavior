rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat_msg <- data.table::fread("C:\\Users\\Administrator\\Documents\\02_Prevention\\02_ZweitesArbeitspaket\\Auswertung\\Data\\EyeLink\\msg.csv", sep = ";", dec = ",")
dat_fixa <- data.table::fread("C:\\Users\\Administrator\\Documents\\02_Prevention\\02_ZweitesArbeitspaket\\Auswertung\\Data\\EyeLink\\fixa.csv", sep = ";", dec = ",")
dat_sacc <- data.table::fread("C:\\Users\\Administrator\\Documents\\02_Prevention\\02_ZweitesArbeitspaket\\Auswertung\\Data\\EyeLink\\sacc.csv", sep = ";", dec = ",")


# excluded due to too less responses
# vp10, vp11, vp12, vp15, vp17, vp27
# excluded due to invalid baseline
# vp12, vp34, vp51

dat_msg <- dat_msg %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")
dat_fixa <- dat_fixa %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")
dat_sacc <- dat_sacc %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")


# merge counterbalance files
## Params:
# Directory of the data
#dir <- paste0(c(getwd(), ""), collapse = "")
dir <- "C:\\Users\\Administrator\\Documents\\02_Prevention\\02_ZweitesArbeitspaket\\Auswertung\\Data\\Analyse\\prot\\"
# Regex matching the Tracking data file names
filename_pattern <- "vp\\d{2,}\\_BL.csv"


# <CODE>
file_list <- list.files(path = dir, pattern = filename_pattern, full.names = T, recursive = T, no.. = T)
dat_seq <- rainR::merge_files(file_list, dec = ",")
# </CODE>

dat_seq <- dat_seq %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")

dat_seq <- dat_seq %>% rename(OpPrev = valence, ActPass = operant)

dat_msgfilt <- dat_msg %>% filter(event != "Cue")

dat <- full_join(dat_seq, dat_sacc, by = c("vp", "trial"))
dat <- full_join(dat, dat_msgfilt, by = c("vp", "trial"))

dat <- dat %>% relocate(c(vp, trial, OpPrev, ActPass))
dat <- dat %>% relocate(c(fileName, path), .after = event)

dat <- dat %>% mutate(
  OpPrev = ifelse(OpPrev == 1, "operant", "prevention"),
  ActPass = ifelse(ActPass == 1, "active", "passive")
)

###
# Bis hier hin wird das Datenfile so vorbereitet, dass alles in einem Datenfile ist und dass es schön ist
###

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



#-------
#-------
#-------
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

#------
# Berechnung interessantes Zeitintervall

dat <- dat %>% mutate(
  #relsacc = ifelse((timest >= time) & (timeend >= time), 1, 0)
  relsacc = ifelse((timest >= time - 600) & (timeend <= time), 1, 0),
  saccdur = timeend - timest
)

#------
# exclusion due to < 10 observations per cell
# exclusion Fehler und Baseline ok
dat <- dat %>% filter(vp != "vp21" & vp != "vp22" & vp != "vp35" & vp != "vp37" & vp != "vp49")
dat <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% filter(correct_trial == 1 & blok == 1)

# exclusion, wenn Sakkade zu lang dauert
dat <- dat %>% filter(saccdur < 200)

#Koordinaten noch falsch rum, deshalb so
# dat <- dat %>% mutate(
#   direction = ifelse(yst > yend, 1, 0)
# )
# 
# dat <- dat %>% filter(direction == 1)

dat_saccnumb <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% summarize(
  sacc_sum = n(),
  relsacc_sum = sum(relsacc)
)

dat_saccnumb <- dat_saccnumb %>% mutate(
  relsacc_sum = ifelse(relsacc_sum > 0, 1, 0)
)

dat_saccnumb <- dat_saccnumb %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
  percent = sum(relsacc_sum)/ n()
)

# dat_saccnumb <- dat %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
#   sacc_sum = n(),
#   relsacc_sum = mean(relsacc)
# )

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


# 
# dat_saccnumb %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
#   mean_relsacc = mean(relsacc_sum),
#   sd_relsacc = sd(relsacc_sum),
#   
# )
# 
# dat_saccnumb %>% ungroup() %>% group_by(OpPrev) %>% summarize(
#   mean_relsacc = mean(relsacc_sum),
#   sd_relsacc = sd(relsacc_sum)
# )
# 
# dat_saccnumb %>% ungroup() %>% group_by(ActPass) %>% summarize(
#   mean_relsacc = mean(relsacc_sum),
#   sd_relsacc = sd(relsacc_sum)
# )
# 
# 
# 
# dat_saccnumb <- dat_saccnumb %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
#    relsacc_sum = mean(relsacc_sum)
# )


t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
t.test(percent ~ ActPass, data= (dat_saccnumb %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(percent ~ OpPrev, data= (dat_saccnumb %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevention" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()



#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


dat <- dat %>% ungroup() %>% group_by(vp, trial) %>% filter(relsacc == 1) %>% filter(row_number() == 1)

dat <- dat %>% ungroup() %>% mutate(
  x_st = xst-blmeanx,
  y_st = yst-blmeany,
  x_end = xend -blmeanx,
  y_end = yend -blmeany
)


#warum so rum? wo sind welche Koordinaten auf dem Bildschirm?
dat <- dat %>% mutate(
  saccade_length = y_st - y_end
) %>% filter(!is.na(saccade_length))


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


dat_aggr <- dat %>% ungroup() %>% group_by(vp, OpPrev, ActPass) %>% summarize(
  mean_saclen  = mean(saccade_length),
  sd_saclen  = sd(saccade_length)
)

dat_aggr %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
  mean_saclen  = mean(mean_saclen)
  )

dat_aggr <- dat_aggr %>% mutate_at(c("vp", "OpPrev", "ActPass"), as_factor)

ez::ezANOVA(data = dat_aggr, dv = mean_saclen, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()

# aggr_saclen <- dat_aggr %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
#   mean_saclen  = mean(mean_saclen)
# )


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

ggplot(data = dat_saclen_join, mapping = aes(x = OpPrev, y = m_relsacc, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = m_relsacc - se_relsacc, ymax = m_relsacc + se_relsacc), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Mean length of first saccade") +
  theme_classic()


# dat_aggr %>% ungroup() %>% group_by(OpPrev, ActPass) %>% summarize(
#   mean_saclen = mean(mean_saclen),
#   sd_saclen = sd(sd_saclen)
# )
# 
# dat_aggr %>% ungroup() %>% group_by(OpPrev) %>% summarize(
#   mean_saclen = mean(mean_saclen)
# )
# 
# dat_aggr %>% ungroup() %>% group_by(ActPass) %>% summarize(
#   mean_saclen = mean(mean_saclen)
# )

t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(mean_saclen ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "prevention")), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ ActPass, data= (dat_aggr %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter((OpPrev == "prevention" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(mean_saclen ~ OpPrev, data= (dat_aggr %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevention" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()

