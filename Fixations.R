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

# use only last fixation and only trials with valid baseline
lastprotfixa <- protfixa %>%
  group_by(vp, trial)%>%
  filter(blok==1)%>%
  filter(row_number()== n())%>%
  ungroup()

# correct for individual baselines
lastprotfixa <- lastprotfixa %>%
  mutate(x = x-blmeanx,
         y = (y-blmeany)*(-1)) %>%
  filter(case_when(operant==0 ~ response == 0,  #filter out false or multiple responses
                   operant==1 ~ response == 1)
  )%>%
  mutate(OpPrev = ifelse(valence == 1,"operant", "prevent"),
         ActPass = ifelse(operant == 1, "active", "passive")) 

plotlastfixa <- lastprotfixa %>%
  group_by(OpPrev, ActPass)%>%
  summarise(
    mean = mean(y),
    sd = sd(y),
    se = se(y),
    percent_missing = 1-(length(trial)/(vpn.n*50))
  )


ez::ezANOVA(data = lastprotfixa, dv = y, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out()

ggplot(data = plotlastfixa, mapping = aes(x = OpPrev, y = mean, group = ActPass, color = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Distance of last Fixation during Anticipation from Cue-Position +- SE") +
  theme_classic()

lastprotfixa_t.test <- lastprotfixa %>% group_by(vp, OpPrev, ActPass) %>%
  summarise(y = mean(y))

t.test(y ~ OpPrev, data= (lastprotfixa_t.test %>% filter(ActPass == "passive")), paired = T) %>% schoRsch::t_out()
t.test(y ~ OpPrev, data= (lastprotfixa_t.test %>% filter(ActPass == "active")), paired = T) %>% schoRsch::t_out()

t.test(y ~ ActPass, data= (lastprotfixa_t.test %>% filter(OpPrev == "prevent")), paired = T) %>% schoRsch::t_out()
t.test(y ~ ActPass, data= (lastprotfixa_t.test %>% filter(OpPrev == "operant")), paired = T) %>% schoRsch::t_out()

t.test(y ~ OpPrev, data= (lastprotfixa_t.test %>% filter((OpPrev == "prevent" & ActPass == "active")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(y ~ OpPrev, data= (lastprotfixa_t.test %>% filter((OpPrev == "operant" & ActPass == "active")|(OpPrev == "prevent" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()


# Percent of Fixations that lies in upper half of screen 

fixaupperhalf <- lastprotfixa %>%
  mutate(upperhalf = ifelse(y > 150, 1,0)) 

fixaupperhalf %>%
  group_by(ActPass, OpPrev) %>%
  summarise(
    percent = sum(upperhalf)/length(upperhalf)
  )

# Percent of fixations that lies within picture (anticipation)

fixapicture <- lastprotfixa %>% ungroup()%>%
  mutate(picture_y = ifelse((y >= 212.5 & y <= 587.5),1,0),       #y-coordinates of picture (if cue is origin) picture: 500 x 375 pixels, center 400 pixels away from cue center
         picture_x = ifelse((x >= -250 & x <= 250),1,0),          #x-coordinates of picture (if cue is origin)
         picture = ifelse((picture_x == 1 & picture_y == 1),1,0)) 

fixapicture %>%
  group_by(ActPass, OpPrev) %>%
  summarise(
    percent = sum(picture)/length(picture)
  )



## Percent of fixation that lies within picture (during onset) - operant active and prevention passive only --------------------------------------------------------------------------------

picturefixa <- data.frame()
protpicturefixa <- data.frame()

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
      filter(timeend >= onsetblock$time & timest <= onsetblock$time+2700)
    fixablock <- fixablock %>%
      mutate(timeend = ifelse (timeend > onsetblock$time+2700, onsetblock$time+2700, fixablock$timeend))
    fixablock <- fixablock %>%
      mutate(timest = ifelse (timest < onsetblock$time, onsetblock$time, fixablock$timest))  
    
    # write dataframe with anticipatory saccades
    picturefixa <- bind_rows(picturefixa,fixablock)
  }
  
  # Combine with trial-info
  #seqdat <- read.csv2(paste("Data/Analyse/prot/",vpn,"_BL.csv",sep=""))
  #seqdatall <- bind_rows(seqdatall,seqdat)
  protpicturefixa <- merge(seqdatall, picturefixa, by = c("vp", "trial"))
}

protpicturefixa <- protpicturefixa %>%
  arrange(vp, trial)

# use only trials with valid baseline
protpicturefixa <- protpicturefixa %>%
  group_by(vp, trial)%>%
  filter(blok==1)%>%
  ungroup()

# use only trials where picture is shown
protpicturefixa <- protpicturefixa %>%
  filter((valence==1 & operant ==1 & response == 1)|(valence == 0 & operant == 0 & response == 0)|(valence == 0 & operant == 1 & response == 0))  #positv aktiv, aversiv passiv = hier kommen Bilder

# correct for individual baselines
protpicturefixa <- protpicturefixa %>%
  mutate(x = x-blmeanx,
         y = (y-blmeany)*(-1)) %>%
  #filter(case_when(operant==0 ~ response == 0,  #filter out false or multiple responses
                   #operant==1 ~ response == 1)
  #)%>%
  mutate(OpPrev = ifelse(valence == 1,"operant", "prevent"),
         ActPass = ifelse(operant == 1, "active", "passive")) 

# fixation lies within picture region? 
protpicturefixa <- protpicturefixa %>% ungroup()%>%
  mutate(picture_y = ifelse((y >= 212.5 & y <= 587.5),1,0),       #y-coordinates of picture (if cue is origin) picture: 500 x 375 pixels, center 400 pixels away from cue center
         picture_x = ifelse((x >= -250 & x <= 250),1,0),          #x-coordinates of picture (if cue is origin)
         picture = ifelse((picture_x == 1 & picture_y == 1),1,0)) %>%
  mutate(duration = timeend-timest) %>%
  mutate(weighted_fixation = picture * duration) %>%
  group_by(vp, trial)%>%
  mutate(sum_duration = sum(duration),
         sum_fixation = sum(weighted_fixation),
         percent = sum_fixation/sum_duration)

plotpicturefixa <- protpicturefixa %>%
  group_by(ActPass, OpPrev) %>%
  summarise(
    mean_percent = mean(percent),
    sd = sd(percent),
    se = se(percent)
  )%>%
  mutate(
    Condition = case_when(ActPass == "active" & OpPrev == "operant" ~ "active operant",
                          ActPass == "active" & OpPrev == "prevent" ~ "active (not) prevent",
                          ActPass == "passive" & OpPrev == "prevent" ~ "passive prevent")
  )

plotpicturefixa_test <- protpicturefixa %>%
  group_by(vp, OpPrev, ActPass) %>%
  summarise(
    mean_percent = mean(percent),
    sd = sd(percent),
    se = se(percent)
  )%>%
  mutate(
    Condition = case_when(ActPass == "active" & OpPrev == "operant" ~ "active operant",
                          ActPass == "active" & OpPrev == "prevent" ~ "active (not) prevent",
                          ActPass == "passive" & OpPrev == "prevent" ~ "passive prevent")
  )

ggplot(data = plotpicturefixa, mapping = aes(x = Condition, y = mean_percent, fill = ActPass)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_percent - se, ymax = mean_percent + se), width = 0.05) +
  geom_violin(data = plotpicturefixa_test, mapping=aes(x= Condition, y = mean_percent, fill = ActPass), alpha = .2) +
  scale_color_manual(labels = c("active", "passive"), values = c("#6BBFA3", "#007AC3")) +
  ylab("Percent of Time spent looking at picture +- SE") +
  theme_classic()

t.test(mean_percent ~ ActPass, data = plotpicturefixa_test %>% filter(Condition == "active operant" | Condition == "passive prevent"), paired = T) %>% schoRsch::t_out()


