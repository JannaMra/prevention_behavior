  rm(list=ls()) 
  options(digits = 7)
  library(tidyverse)
  library(data.table)
  
  dat <- data.table::fread("dat2.csv", sep = ",", dec = ".")
  
  # Berechnung interessantes Zeitintervall
  
  dat <- dat %>% ungroup() %>% group_by(ActPass, response) %>% mutate(
    correct_trial = ifelse((ActPass == "active" & response == 1) | (ActPass == "passive" & response == 0), 1, 0)
  )
  
  dat <- dat %>% mutate(
    relsacc = ifelse((timeend > time), 1, 0),
    saccdur = timeend - timest,
    y = (y - blmeany) * (-1)
  )

#------
# exclusion Fehler und Baseline ok
  dat <- dat %>% filter(vp != "vp21" & vp != "vp22" & vp != "vp35" & vp != "vp37" & vp != "vp49")
dat <- dat %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% filter(correct_trial == 1 & blok == 1) %>% filter(relsacc == 1) %>% filter(row_number()<=6)


  
  
  dat_s <- dat
  
  dat_s <- dat_s %>% ungroup() %>% group_by(vp, trial, OpPrev, ActPass) %>% mutate(
    fix_number = row_number()
  )
  
  dat_saccnumb <- dat_s %>% ungroup() %>% group_by(vp, OpPrev, ActPass, fix_number) %>% summarize(
    mean_x = mean(x),
    mean_y = mean(y)
  )
  

  dat_saccnumb <- dat_saccnumb %>% mutate(
    fourgroup = ifelse(OpPrev == "operant" & ActPass == "active", "OpAct", NA)
  ) %>% mutate(
    fourgroup = ifelse(OpPrev == "operant" & ActPass == "passive", "OpPas", fourgroup)
  )  %>% mutate(
    fourgroup = ifelse(OpPrev == "prevention" & ActPass == "active", "PrevAct", fourgroup)
  ) %>% mutate(
    fourgroup = ifelse(OpPrev == "prevention" & ActPass == "passive", "PrevPass", fourgroup)
  )
  
  
  dat_saccnumb_filt <- dat_saccnumb %>% filter(fourgroup == "OpPas" | fourgroup == "PrevAct")  
  
  ez::ezANOVA(data = dat_saccnumb_filt, dv = mean_y, wid = vp, within = c(fourgroup, fix_number), detailed = TRUE) %>% schoRsch::anova_out() 
  
  
  
 dat_saccnumb <- dat_saccnumb %>% mutate_at(c("vp", "OpPrev", "ActPass", "fix_number", "fourgroup"), as_factor)
  
  ez::ezANOVA(data = dat_saccnumb, dv = mean_y, wid = vp, within = c(OpPrev, ActPass, fix_number), detailed = TRUE) %>% schoRsch::anova_out()

  ez::ezPlot(data = dat_saccnumb, dv = mean_y, wid = vp, within = c(fourgroup, fix_number), x = fix_number, split = fourgroup)

  
  
  
  #################################################################################################################################
