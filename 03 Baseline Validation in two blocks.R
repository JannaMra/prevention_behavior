############################################################################
# ShockScanning_Flight Oscillations project
# 
# - Score baselines and determine baseline quality
#

path.prot <- "Data/Analyse/prot/" %>% paste0(path, .)
savepath <- "Data/Analyse/BL/" %>% paste0(path, .)


# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -300; blen <- 0
# Stimulus duration
dur <- 10000

# Plot individual baselines to jpg?
plotBL1 <- TRUE
plotBL2 <- TRUE

# Load fixation list
#fixa <- read.table(paste(path,"Data/Eyelink/Prevention/Output/Fixations.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".")
fixa <- read.table(paste(path,"Data/Eyelink/Prevention_Follow_up/Output/Fixations.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".")
names(fixa) <- c("vp","trial","timest","timeend","x","y")
# have trials as numeric from 1:90 
fixa$trial <- as.numeric(sub("Trial: ", "", fixa$trial))
fixa$trial <- ifelse(grepl("_2",fixa$vp), 
                     fixa$trial+100, 
                     fixa$trial)
fixa$vp <- sub("_1", "",fixa$vp)
fixa$vp <- sub("_2", "",fixa$vp)
write.csv2(fixa, paste0(path,"Data/Eyelink/fixa.csv"))
fixablock <- fixa %>% mutate(block = case_when(
  trial <= 100 ~ 1,
  trial>100  ~ 2))

# Load saccade list
#sacc <- read.table(paste(path,"Data/Eyelink/Prevention/Output/Saccades.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".") #Experiment 1
sacc <- read.table(paste(path,"Data/Eyelink/Prevention_Follow_up/Output/Saccades.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".") #Experiment 2
names(sacc) <- c("vp","trial","blink","timest", "timeend","xst","yst", "xend", "yend")
# have trials as numeric from 1:90 
sacc$trial <- as.numeric(sub("Trial: ", "", sacc$trial))
sacc$trial <- ifelse(grepl("_2",sacc$vp), 
                     sacc$trial+100, 
                     sacc$trial)
sacc$vp <- sub("_1", "",sacc$vp)
sacc$vp <- sub("_2", "",sacc$vp)
write.csv2(sacc, paste0(path,"Data/Eyelink/sacc.csv"))

# Get onsets
#msg <- read.table(paste(path,"Data/Eyelink/Prevention/Output/Messages.txt",sep=""),sep = '\t', skip = 1, dec=".", na.strings=".", 
                  #colClasses=c("character","character","numeric", "character"))
msg <- read.table(paste(path,"Data/Eyelink/Prevention_Follow_up/Output/Messages.txt",sep=""),sep = '\t', skip = 1, dec=".", na.strings=".", 
                  colClasses=c("character","character","numeric", "character"))
names(msg) <- c("vp","trial","time", "event")
msg$trial <- as.numeric(sub("Trial: ", "", msg$trial))
msg$trial <- ifelse(grepl("_2",msg$vp), 
                    msg$trial+100, 
                    msg$trial)
msg$vp <- sub("_1", "",msg$vp)
msg$vp <- sub("_2", "",msg$vp)
msg$event <- sub("Stimulus ", "",msg$event)
msg <- msg %>%
  filter(event!= "!MODE RECORD CR 500 2 1 R")
write.csv2(msg, paste0(path,"Data/Eyelink/msg.csv"))
msg <- msg %>%
  filter(event == "Cue")

# Onsets laden
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1)
msg <- msg %>%
  filter(event == "Cue")
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
fixablock <- fixa %>% mutate(block = case_when(
  trial <= 100 ~ 1,
  trial>100  ~ 2))
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

# Determine which subjects should be analyzed
vpn = fixa$vp %>% unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
vpn.n = length(vpn)

# Exclusions:
vpn <-  vpn[!(vpn %in% c())]
vps=vpn

# Iterative outlier removal
outlier_remove <- function(x,sdmult=3) {
  # Remove outlier iteratively
  insample <- rep(1,length(x)); insample[is.na(x)] <- 0
  ok <- FALSE
  while (!ok) {
    xminpos <- (1:length(x))[x==min(x[insample==1])]
    xminpos <- xminpos[xminpos %in% (1:length(insample))[insample==1]][1]
    xmaxpos <- (1:length(x))[x==max(x[insample==1])]
    xmaxpos <- xmaxpos[xmaxpos %in% (1:length(insample))[insample==1]][1]
    tempinsample <- insample; tempinsample[c(xminpos,xmaxpos)] <- 0
    subx <- x[tempinsample==1]
    if (x[xminpos]<(mean(subx)-sdmult*sd(subx))) {
      insample[xminpos] <- 0
      out1 <- TRUE
    } else {
      out1 <- FALSE
    }
    
    if (x[xmaxpos]>(mean(subx)+sdmult*sd(subx))) {
      insample[xmaxpos] <- 0
      out2 <- TRUE
    } else {
      out2 <- FALSE
    }
    
    if (!out1 & !out2) { ok <- TRUE }
  }
  return(insample)
}


bl_quality <- data.frame()

# Loop over subjects
for (vpn in vps) {
  
  prot <- read.csv2(paste(path.prot,vpn,".csv",sep=""),header=TRUE)
  prot$blx  <- NA
  prot$bly  <- NA
  prot$blok1 <- NA
  prot$blok2 <- NA
  prot$blok <- NA
  
  blxok_1 <- rep(0,100)
  blyok_1 <- rep(0,100)
  blxok_2 <- rep(0,100)
  blyok_2 <- rep(0,100)
  
  baseline <- numeric()  # Position (x/y)
  validfix <- numeric()  # Valid fixation time (ms), excluding 1st fixation
  print(vpn)
  
  code <- vpn
  
  ###########################################
  # 1. Determine baselines
  # Generate empty data field to store data
  
  # Determine trial number
  vpfix <- fixablock[tolower(fixa$subject)==code,]
  #ntrial <- nrow(prot)
  
  # Loop over trials to determine trial-by-trial baselines
  for(blockid in 1:2){
    print(blockid)
    baseline <- numeric()
    if(blockid == 1) {
      trialid = 1
      ntrial = 100
    }
    if(blockid == 2) {
      trialid = 101
      ntrial = 200
    }
       
    for(trialid in trialid:ntrial){
    # Select trial data
      fixblock <- fixablock%>%
        filter(trial == trialid &
               vp == vpn &
               block == blockid)
      msgblock <- msg [
        tolower(msg$vp)==code &
          msg$trial==trialid,]
    
    # Check if correct Stimulus marker is present in MSG file
    # if (msgblock$event!=paste(prot$pic[trial],
    #                           ".jpg",sep="")) {
    #   print(paste("Stimulus error: Trial:",trial," Event:",msgblock$event,sep=""))
    # }
    
    # Determine onset (in ms)
    onset <- as.numeric(msgblock$time)
    
    # Subtract onset from timestamps
    fixblock$start  <- as.numeric(fixblock$timest)-onset
    fixblock$end <- as.numeric(fixblock$timeend)-onset
    
    # Cut last fixation to presentation time
    fixblock$end[nrow(fixblock)] <- 
      ifelse(
        fixblock$end[
          nrow(fixblock)]>dur,
        dur,
        fixblock$end[
          nrow(fixblock)])
    
    
    # Calculate valid fixation time
    fixblock$dur <- as.numeric(fixblock$end) - as.numeric(fixblock$start)
    validfix <- c(validfix,sum(fixblock$dur[fixblock$start>0],na.rm=TRUE))
    
    # Calculate baseline as weighted average of fixations
    fixblockbl <- fixblock[fixblock$end>blst & fixblock$start<blen,]
    if (nrow(fixblockbl)>0) {
      # Restrict fixation data to baseline
      fixblockbl$start[1] <- ifelse(head(fixblockbl$start,1)<blst,blst,head(fixblockbl$start,1))
      fixblockbl$end[nrow(fixblockbl)] <- ifelse(tail(fixblockbl$end,1)>blen,blen,tail(fixblockbl$end,1))
      
      
      # Calculate baseline coordinates
      xbl <- sum(fixblockbl$x*fixblockbl$dur)/sum(fixblockbl$dur)
      ybl <- sum(fixblockbl$y*fixblockbl$dur)/sum(fixblockbl$dur)
      
      # Store values
      baseline <- rbind(baseline,c(xbl,ybl))
    } else {
      # When no valid fixations are available store NA as baseline for current trial
      baseline <- rbind(baseline,c(NA,NA))
    }
    }
    if(blockid == 1) {
      baseline_1 <- baseline}
    if(blockid == 2) {
      baseline_2 <- baseline}
  }
  
  # Determine outlier (only perform baseline validation if less than 100 NAs/ at least 10 valid fixations)
  
  if (!sum(is.na(baseline_1))> 100) {
    blxok_1 <- outlier_remove(baseline_1[,1])
    blyok_1 <- outlier_remove(baseline_1[,2])
  } else {print("Too little values for Baseline Validation in Block 1")}
  
  if (!sum(is.na(baseline_2))> 100) {
    blxok_2 <- outlier_remove(baseline_2[,1])
    blyok_2 <- outlier_remove(baseline_2[,2])
  } else {print("Too little values for Baseline Validation in Block 2")}
  
  # Baseline is valid when x and y coordinates are ok (i.e. no outlier)
  blok_1 <- as.numeric((blxok_1==1) & (blyok_1==1))
  blok_2 <- as.numeric((blxok_2==1) & (blyok_2==1))
  
  ##NEW AM Error: in $ data frame ... replacement has 183 rows, data has 60
  #prot$blx <- cut(prot$blx, baseline[,1])
  #prot$bly <- baseline[,2]
  
  prot$blx <- c(baseline_1[,1], baseline_2[,1])
  prot$bly <- c(baseline_1[,2], baseline_2[,2])
  prot$blok1 <- c(blok_1, blok_2) # outlier because more than 3 SD from mean 
  prot$validfix <- validfix
  # hier weiter
  meanx1 <- rep(mean(baseline_1[,1],na.rm = TRUE), 100)
  meanx2 <- rep(mean(baseline_2[,1],na.rm = TRUE), 100)
  meanx <- c(meanx1, meanx2)
  prot$blmeanx <- meanx
  meany1 <- rep(mean(baseline_1[,2],na.rm = TRUE), 100)
  meany2 <- rep(mean(baseline_2[,2],na.rm = TRUE), 100)
  meany <- c(meany1, meany2)
  prot$blmeany <- meany

  prot <- prot %>%
    mutate(blok2 = case_when(abs(blmeanx - blx) > 50 | abs(blmeany - bly) > 50 ~ 0, #outlier because more than 150 pixels from mean
                             abs(blmeanx - blx) < 50 & abs(blmeany - bly) < 50 ~ 1
    )
    )
  prot <- prot %>%
    mutate(blok = case_when(blok1 == 0 | blok2 == 0 ~ 0, # outlier because either more than 3 SD or more than 150 pixels away
                            blok1 == 1 & blok2 == 1 ~ 1))
  blok <- prot$blok
  
  prot1 <- head(prot,100)
  prot2 <- tail(prot,100)
  
  # Testplot
  
  if (plotBL1) {
    png(paste(savepath,vpn,"_1",".jpg",sep=""),width=500,height=500,pointsize=18)
    plot(prot1$blx,prot1$bly,pch=16,col="black",xlab="x (px)",ylab="y (px)",xlim=c(0,1920),ylim=c(1080,0))
    points(prot1%>% filter(blok==0) %>% select(blx,bly),pch=16,col="red")
    title(paste0(vpn,"_1"))
    dev.off()
  }
  if (plotBL2) {
    png(paste(savepath,vpn,"_2",".jpg",sep=""),width=500,height=500,pointsize=18)
    plot(prot2$blx,prot2$bly,pch=16,col="black",xlab="x (px)",ylab="y (px)",xlim=c(0,1920),ylim=c(1080,0))
    points(prot2%>% filter(blok==0) %>% select(blx,bly),pch=16,col="red")
    title(paste0(vpn,"_2"))
    dev.off()
  }
  
  # Store number of valid baselines per subject
  bl_quality_vp <- data.frame(nrow(prot),sum(blok))
  
  # Store spread of baselines in x and y direction
  # xrng <- max(baseline[blok==1,1])-min(baseline[blok==1,1])
  # yrng <- max(baseline[blok==1,2])-min(baseline[blok==1,2])
  # bl_quality_vp <- c(bl_quality_vp,xrng,yrng)
  
  write.csv2(prot,paste(path.prot,vpn,"_BL.csv",sep=""),row.names=FALSE,quote=FALSE)
  
  bl_quality <- rbind(bl_quality,bl_quality_vp)
  
}

erg <- data.frame(vps,bl_quality,row.names=NULL)
names(erg) <- c("vp","alltrials","blok")

# Mark problematic cases
erg$pblok <- erg[,3]/erg[,2]
erg$prob <- ifelse(erg$pblok<= 0.5,1,0)
sum(erg$prob)
#erg$problem  <- as.numeric((erg$xrng>150) | (erg$yrng>150))
write.csv2(erg,paste(path.prot,"Results_BaselineCheck_blockwise.csv",sep=""),row.names=FALSE,quote=FALSE)

# Define cases with too many outliers (>50%) to exclude from further analyses
eye.invalid.bl <- erg %>% filter(prob == 1) %>% pull(vp) %>% unique()

