############################################################################
# Prevention Behavior project
# 
# 

library(tidyverse)
path.log <- "Data/log/" %>% paste0(path, .) 
path.seq <- "Data/sequences/" %>% paste0(path, .) 
savepath <- "Data/Analyse/prot/" %>% paste0(path, .)  


vpn <- paste("vp",ifelse(4:34<10,"0",""),4:34,sep="")
#vpn <- "vp04"
#vpn <- "vp01"
# Exclusions:
#vpn <- vpn[!(vpn %in% c(e.g.,"vpbs01"))]

#names(daten)
# [1] "Subject"       "Trial"         "Event.Type"   
# [4] "Code"          "Time"          "TTime" --> Reaktionszeit       
# [7] "Uncertainty"   "Duration"      "Uncertainty.1"
#[10] "ReqTime"       "ReqDur"  

responses.summary <- data.frame()

for (vp in vpn) {
  print(vp)
  
  seqdat1 <- read.table(paste(path.seq,vp,"_1.txt",sep=""),header=TRUE)
  seqdat2 <- read.table(paste(path.seq,vp,"_2.txt",sep=""),header=TRUE)
  seqdat <- bind_rows(seqdat1,seqdat2)
  seqdat$vp <- vp
  seqdat$trial <- c(1:200)
  
  seqdat$rt        <- NA  # Reaktionszeit
  
  
  # Log einlesen
  header.logdat <- read.table(file=paste(path.log,vp,"_1-PreventionBehavior.log",sep=""), skip = 3, 
                              nrows = 1, header = FALSE, sep ="\t", stringsAsFactors = FALSE)
  logdat1  <- read.table(file=paste(path.log,vp,"_1-PreventionBehavior.log",sep=""), 
                        sep="\t", skip = 4, header=FALSE,fill=TRUE)
  logdat2  <- read.table(file=paste(path.log,vp,"_2-PreventionBehavior.log",sep=""), 
                         sep="\t", skip = 8, header=FALSE,fill=TRUE)     # skip 8, so that "responses" are not counted as responses to cue
  logdat <- bind_rows(logdat1, logdat2)
  header.logdat[3]<- "Event.Type"
  colnames( logdat ) <- unlist(header.logdat)
  
  aktstim <- 1
  #aktstim <- 3
  i <- 1
  
  while (i < nrow(logdat)) {
    
    
    #nimm pro reihe des log files nur die, in denen ein Cue zu finden ist
    if (as.character(logdat$Code[i])=="cue") {
      
      
      # Find next stimulus
      j <- i + 1
      
      
      #wenn kein Cue, geh eine Reihe Weiter
      while ((j < nrow(logdat)) & (as.character(logdat$Code[j])!="cue")) {
        
        j <- j + 1
      }
      
      
      #eine reihe vor jpg bis zwei reihen vor nächstem jpg ist ein Stimblock
      stimblock <- logdat[(i):(j-1),] 
      
      
      # Condition korrekt 
      #if (stimblock$Code[1]!=c("flight", "shock", "noshock")[seqdat$cue[aktstim]+1]) {
      #  print("Wrong condition")
      #}
      
      # Response in active Trial?
      seqdat$response[aktstim] <- sum(stimblock$Event.Type=="Response") #wenn Reaktion erfolgt ist, dann 1 in Spalte eintragen
      
      # response time in trials
      # erstmal flight trials suchen
      active_markers <- which(stimblock$Event.Type=="Response")

      ifelse(length(active_markers)>0, seqdat$rt[aktstim] <- stimblock$TTime[active_markers]/10, seqdat$rt[aktstim] <- NA)
      
      # if (length(active_markers)>0) {
      #   #druck die TTime in die entsprechende Zeile der seqdat
      #   seqdat$rt[aktstim] <- stimblock$TTime[active_markers]/10
      # }

      #if (seqdat$cue[aktstim] > 0) {
      #   #only reaction times in flight phase are documented in prot-file
      #   seqdat$rt[aktstim] = NA
      # }
        
      aktstim <- aktstim+1
    }
    i <- i+1
  }
  
  write.csv2(seqdat,paste(savepath,vp,".csv",sep=""),row.names=FALSE,quote=FALSE)
  
  # Testberechnungen
  #print(table(seqdat$operant,seqdat$response))
  # Anzahl Trials mit fehlerhaften Reaktionen
  print(paste0("No responses in active prevention trials: ",sum(seqdat$operant==1 & seqdat$valence==0 & seqdat$response==0)))
  print(paste0("No responses in active creation trials: ",sum(seqdat$operant==1 & seqdat$valence==1 & seqdat$response==0)))
  print(paste0("Response in passive trials: ",sum(seqdat$operant==0 & seqdat$response==1)))
  seqdat <- seqdat %>% 
    mutate(problem = case_when(
      operant==1 & response==0 | operant==0 & response==1 ~ 1,
      operant==1 & response==1 | operant==0 & response==0 ~ 0)
      )
  
  missing.prevention <- sum(seqdat$operant==1 & seqdat$valence==0 & seqdat$response==0)
  missing.creation <- sum(seqdat$operant==1 & seqdat$valence==1 & seqdat$response==0)
  reaction.passive <- sum(seqdat$operant==0 & seqdat$response==1)
  summary.vp <- data.frame(vp, missing.prevention, missing.creation,reaction.passive)
  responses.summary <- bind_rows(responses.summary, summary.vp)
}

# list of exclusions
print(exclusion.responses <- responses.summary %>% filter(missing.prevention/50 > 0.5 | missing.creation/50 > 0.5) %>% .$vp %>% unique())







### nochmal zum Laufen bringen für incorrect und premature responses 

# for (vp in vpn) {
#   print(vp)
#   
#   seqdat1 <- read.table(paste(path.seq,vp,"_1.txt",sep=""),header=TRUE)
#   seqdat2 <- read.table(paste(path.seq,vp,"_2.txt",sep=""),header=TRUE)
#   seqdat <- bind_rows(seqdat1,seqdat2)
#   
#   seqdat$resp   <- NA  # Reaktion (Taste)
#   seqdat$rt     <- NA  # Reaktionszeit
#   seqdat$shock  <- 0   # Schock verabreicht?
#   
#   # Log einlesen
#   # Log einlesen
#   header.logdat <- read.table(file=paste(path.log,vp,"_1-ShockScanning_Flight.log",sep=""), skip = 3, 
#                               nrows = 1, header = FALSE, sep ="\t", stringsAsFactors = FALSE)
#   logdat1  <- read.table(file=paste(path.log,vp,"_1-ShockScanning_Flight.log",sep=""), 
#                          sep="\t", skip = 4, header=FALSE,fill=TRUE)
#   logdat2  <- read.table(file=paste(path.log,vp,"_2-ShockScanning_Flight.log",sep=""), 
#                          sep="\t", skip = 4, header=FALSE,fill=TRUE)
#   logdat <- bind_rows(logdat1, logdat2)
#   header.logdat[3]<- "Event.Type"
#   colnames( logdat ) <- unlist(header.logdat)
#   
#   aktstim <- 1
#   #aktstim <- 2
#   i <- 1
#   while (i < nrow(logdat)) {
#     if (as.character(logdat$Code[i])==paste(seqdat$pic[aktstim],".jpg",sep="")) {
#       # Find next stimulus
#       j <- i
#       while ((j < nrow(logdat)) & (as.character(logdat$Code[j])!=paste(seqdat$pic[aktstim+1],".jpg",sep=""))) {
#         j <- j + 1
#       }
#       
#       # Find cue preceding stimulus
#       cuepos <- i-1
#       while ((cuepos > 1) & !(as.character(logdat$Code[cuepos]) %in% c("flight", "shock", "noshock"))) {
#         cuepos <- cuepos - 1
#       }
#       
#       
#       stimblock <- logdat[cuepos:(j-2),]
#       
#       # Condition korrekt
#       if (stimblock$Code[1]!=c("flight", "shock", "noshock")[seqdat$cue[aktstim]+1]) {
#         print("Wrong condition")
#       }
#       
#       # Shock given?
#       seqdat$shock[aktstim] <- sum(stimblock$Code=="shock1")
#       
#       # Reaktionen finden und speichern
#       flightprompt <- which(stimblock$Code=="flight")
#       for (k in 1:nrow(stimblock)) {
#         # Reaktion (nur die erste Reaktion werten)
#         if ((stimblock$Event.Type[k]=="Response") & (is.na(seqdat$resp[aktstim]))) {
#           reaktion <- as.numeric(as.character(stimblock$Code[k]))
#           rtdat <- stimblock$TTime[k] #stimblock$Time[k]-stimblock$Time[flightprompt] 
#           
#           if (length(flightprompt)==1) {
#             # Response occurred in flight trial
#             seqdat$resp[aktstim]  <- reaktion
#             seqdat$rt[aktstim] <- rtdat/10
#           } else {
#             # Response occurred in other trial
#             seqdat$resp[aktstim]  <- 0
#             rtdat <- stimblock$Time[k]-stimblock$Time[1] 
#             seqdat$rt[aktstim] <- rtdat/10
#           }
#         }    
#       }
#     aktstim <- aktstim+1
#     }
#     i <- i+1
#   }
#   
#   names(seqdat) <- c("pic","cue","iti","resp","rt","shock")
#   write.csv2(seqdat,paste(savepath,vp,".csv",sep=""),row.names=FALSE,quote=FALSE)
#   
#   # Testberechnungen
#   # Anzahl geschockte Trials per Condition
#   print(table(seqdat$cue,seqdat$shock))
#   # Anzahl Trials mit fehlerhaften Reaktionen
#   print(paste0("Erroneous responses: ",sum(seqdat$resp==0,na.rm=TRUE)))
#   # Anzahl Flight-Trials mit zu fruehen Reaktionen
#   print(paste0("Premature flight responses: ",sum(seqdat$resp==2 & seqdat$rt<0,na.rm=TRUE)))
# }

