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

onset <- data.frame()

for (vpn in vps) {
  
  #vpn = "vp05"
  vpcode <- vpn
  code <- vpn
  print(code)
  ntrial <- 200
  seqdat <- read.csv2(paste("Data/Analyse/prot/",vpn,".csv",sep=""))
  
  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    #trial = 1
    
    # Select trial data
    # fixblock <- fixa[
    #   tolower(fixa$vp)==code & #tolower = translates characters in character vectors
    #     fixa$trial==trial,]
    msgblock <- msg [
      tolower(msg$vp)==code &
        msg$trial==trial,]
    
    # Filter 
    msgblock <- msgblock %>%
      filter(event == paste(seqdat$pic[trial],".jpg",sep=""))  
    
    # Check if correct Stimulus marker is present in MSG file
    if (msgblock$event!=paste(seqdat$pic[trial],
                                ".jpg",sep="")) {
      print(paste("Stimulus error: Trial:",trial," Event:",msgblock$event,sep=""))
    }
    
    # write dataframe with onsets
    onset <- bind_rows(onset,msgblock)
  }
}

onset$preonset <- onset$time-600
write.csv2(onset,paste(path,"Data/Eyelink/Onsets.csv",sep=""),row.names=FALSE,quote=FALSE)

antsacc <- data.frame()
seqdatall <- data.frame()

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
    saccblock <- sacc[
      tolower(sacc$vp)==code & #tolower = translates characters in character vectors
        sacc$trial==trial,]
    onsetblock <- onset[
      tolower(onset$vp)==code & #tolower = translates characters in character vectors
      onset$trial==trial,]
    
    # Filter
    saccblock <- saccblock %>%
      filter(timest >= onsetblock$preonset & timeend <= onsetblock$time)
    
    # write dataframe with anticipatory saccades
    antsacc <- bind_rows(antsacc,saccblock)
  }
  # Comine with trial-info
  seqdat <- read.csv2(paste("Data/Analyse/prot/",vpn,"_BL.csv",sep=""))
  seqdatall <- bind_rows(seqdatall,seqdat)
  protsacc <- merge(seqdatall, antsacc, by = c("vp", "trial"))
}

protsacc <- protsacc %>%
  arrange(vp, trial)

# Percent invalid Baselines after exclusion
sum(seqdatall$blok==0)/length(seqdatall$blok)
  

# use only first saccade and only trials with valid baseline
firstprotsacc <- protsacc %>%
  group_by(vp, trial)%>%
  filter(blok==1)%>%
  filter(row_number()==1)%>%
  ungroup()

# correct for individual baselines
firstprotsacc <- firstprotsacc %>%
  mutate(x_st = xst-blmeanx,
         y_st = yst-blmeany,
         x_end = xend -blmeanx,
         y_end = yend -blmeany)%>%
  filter(case_when(operant==0 ~ response == 0,  #filter out false or multiple responses
                   operant==1 ~ response == 1)
  )%>%
  mutate(saccade_length = y_st - y_end)

firstprotsacc %>%
  summarize(
    percent_missing = 1-(length(firstprotsacc$trial)/(vpn.n*200))
  ) 
 
# firstprotsacc %>%
#   group_by(valence, operant) %>%
#   summarize(
#     start_y = mean(yst),
#     end_y = mean(yend),
#     diff= start_y-end_y,
#     percent_missing = 1-(length(trial)/(vpn.n*50))
#   )

# Abhängig von Reaktion/keiner Reaktion 

# firstprotsacc %>%
#   filter(operant == 1) %>%
#   filter(response == 1) %>%
#   group_by(valence) %>%
#   summarize(
#     start_y = mean(yst),
#     end_y = mean(yend),
#     diff = start_y - end_y,
#     percent_missing = 1-(length(trial)/(vpn.n*50))
#   )
# 
# firstprotsacc %>%
#   filter(operant == 0) %>%
#   filter(response == 0) %>%
#   group_by(valence) %>%
#   summarize(
#     start_y = mean(yst),
#     end_y = mean(yend),
#     diff = start_y - end_y,
#     percent_missing = 1-(length(trial)/(vpn.n*50))
#   )

firstprotsacc %>%
  group_by(operant, valence)%>%
  summarise(
    mean = mean(saccade_length),
    sd = sd(saccade_length),
    se = se(saccade_length),
    percent_missing = 1-(length(trial)/(vpn.n*50))
  )

# To do -------------------------
# Evtl. pro Person durchschnittliche Saccade nach oben und dann über alle mitteln? - Solveig
# Wie viel Prozent missings? Wie viele fehlen aufgrund fehlender Reaktionen? - Solveig


