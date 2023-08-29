############################################################################
# Prevention Behavior project
# 
# operant(active):  1 = active, 0 = passive
# valence: 1 = positive, 0 = negative
# group: 1 = pos blue & neg yellow, 2 = pos yellow & neg blue

# Get Onset Picture/no picture

# Onsets laden
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1)
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)
onset <- read.csv2(paste0(path,"Data/Eyelink/Onsets.csv"))

# Determine which subjects should be analyzed
vpn = fixa$vp %>% 
  unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
vpn <-  vpn[!(vpn %in% eye.invalid.bl)]
vpn <-  vpn[!(vpn %in% exclusion.responses)]
vpn <-  vpn[!(vpn %in% c())]
vpn.n = length(vpn)
vps=vpn

antfixa <- data.frame()
protfixa <- data.frame()

for (vpn in vps) {
  #vpn = "vp05"
  vpcode <- vpn
  code <- vpn
  print(code)
  ntrial <- 200
  
  
  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    #trial = 1
    
    # Select trial data
    fixablock <- fixa[
      tolower(fixa$vp)==code & #tolower = translates characters in character vectors
        fixa$trial==trial,]
    onsetblock <- onset[
      tolower(onset$vp)==code & #tolower = translates characters in character vectors
        onset$trial==trial,]
    
    # Filter
    fixablock <- fixablock %>%
      filter(timeend >= onsetblock$preonset & timest <= onsetblock$time)
    
    # write dataframe with anticipatory saccades
    antfixa <- bind_rows(antfixa,fixablock)
  }
  
  # Combine with trial-info
  #seqdat <- read.csv2(paste("Data/Analyse/prot/",vpn,"_BL.csv",sep=""))
  #seqdatall <- bind_rows(seqdatall,seqdat)
  protfixa <- merge(seqdatall, antfixa, by = c("vp", "trial"))
}

protfixa <- protfixa %>%
  arrange(vp, trial)

# use only first saccade and only trials with valid baseline
firstprotfixa <- protfixa %>%
  group_by(vp, trial)%>%
  filter(blok==1)%>%
  filter(row_number()==1)%>%
  ungroup()

# correct for individual baselines
firstprotfixa <- firstprotfixa %>%
  mutate(x = x-blmeanx,
         y = (y-blmeany)*(-1)) %>%
  filter(case_when(operant==0 ~ response == 0,  #filter out false or multiple responses
                   operant==1 ~ response == 1)
  )%>%
  mutate(OpPrev = ifelse(valence == 1,"operant", "prevent"),
         ActPass = ifelse(operant == 1, "active", "passive")) 

plotfirstfixa <- firstprotfixa %>%
  group_by(OpPrev, ActPass)%>%
  summarise(
    mean = mean(y),
    sd = sd(y),
    se = se(y),
    percent_missing = 1-(length(trial)/(vpn.n*50))
  )


ez::ezANOVA(data = firstprotfixa, dv = y, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()

ggplot(data = plotfirstfixa, mapping = aes(x = OpPrev, y = mean, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Distance of last Fixation during Anticipation from Cue-Position +- SE") +
  theme_classic()

firstprotfixa <- firstprotfixa %>% group_by(vp, OpPrev, ActPass) %>%
  summarise(y = mean(y))

t.test(y ~ OpPrev, data= (firstprotfixa %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(y ~ OpPrev, data= (firstprotfixa %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(y ~ ActPass, data= (firstprotfixa %>% filter(OpPrev == "prevent")), paired = T) %>% schoRsch::t_out()
t.test(y ~ ActPass, data= (firstprotfixa %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(y ~ OpPrev, data= (firstprotfixa %>% filter((OpPrev == "prevent" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(y ~ OpPrev, data= (firstprotfixa %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevent" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()


# Percent of Fixations that lies in upper half of screen 

fixaupperhalf <- firstprotfixa %>%
  mutate(upperhalf = ifelse(y > 150, 1,0)) 

fixaupperhalf %>%
  group_by(ActPass, OpPrev) %>%
  summarise(
    percent = sum(upperhalf)/length(upperhalf)
  )
  




