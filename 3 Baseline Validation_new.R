############################################################################
# ShockScanning_Flight Oscillations project
# 
# - Score baselines and determine baseline quality
#

path.prot <- "Analyse/prot/" %>% paste0(path, .)
savepath <- "Analyse/BL/" %>% paste0(path, .)


# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -300; blen <- 0
# Stimulus duration
dur <- 27000

# Plot individual baselines to jpg?
plotBL <- TRUE


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

######## Process ET Data

# Load fixation list
fixa <- read.table(paste(path,"Data/Eyelink/reports/Fixations.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".")
names(fixa) <- c("vp","trial","timest","timeend","x","y")
# have trials as numeric from 1:90 
fixa$trial <- as.numeric(sub("Trial: ", "", fixa$trial))
fixa$trial <- ifelse(grepl("_2",fixa$vp), 
       fixa$trial+100, 
       fixa$trial)
fixa$vp <- sub("_1", "",fixa$vp)
fixa$vp <- sub("_2", "",fixa$vp)
write.csv2(fixa, paste0(path,"Analyse/Eyelink/fixa.csv"))

# Load saccade list
sacc <- read.table(paste(path,"Data/Eyelink/reports/Saccades.txt", sep=""),sep = '\t', skip=1 ,dec=",",na.strings=".")
names(sacc) <- c("vp","trial","blink","timest", "timeend","xst","yst", "xend", "yend")
# have trials as numeric from 1:90 
sacc$trial <- as.numeric(sub("Trial: ", "", sacc$trial))
sacc$trial <- ifelse(grepl("_2",sacc$vp), 
                     sacc$trial+100, 
                     sacc$trial)
sacc$vp <- sub("_1", "",sacc$vp)
sacc$vp <- sub("_2", "",sacc$vp)
write.csv2(sacc, paste0(path,"Analyse/Eyelink/sacc.csv"))

# Get onsets
msg <- read.table(paste(path,"Data/Eyelink/reports/Messages.txt",sep=""),sep = '\t', skip = 1, dec=".", na.strings=".", 
                  colClasses=c("character","character","numeric", "character"))
names(msg) <- c("vp","trial","time", "message")
msg$trial <- as.numeric(sub("Trial: ", "", msg$trial))
msg$trial <- ifelse(grepl("_2",msg$vp), 
                    msg$trial+100, 
                    msg$trial)
msg$vp <- sub("_1", "",msg$vp)
msg$vp <- sub("_2", "",msg$vp)
msg$message <- sub("Stimulus ", "",msg$message)
write.csv2(msg, paste0(path,"Analyse/Eyelink/msg.csv"))


# Determine which subjects should be analyzed
vpn = fixa$vp %>% 
  unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
#vpn <-  vpn[!(vpn %in% eye.invalid.bl)]
vpn.n = length(vpn)
vps=vpn

# Exclusions:
vpn <-  vpn[!(vpn %in% c())]


# Loop Baseline Quality 

bl_quality <- numeric()

#vpn <- "1"
#vps = vpn

# Loop over subjects
for (vpn in vps) {
  #vpn= "1"
  if (plotBL) {
    png(paste(savepath,vpn,".jpg",sep=""),width=500,height=500,pointsize=18)
  }
  
  vpcode <- vpn
  prot <- read.csv2(paste(path.prot,vpcode,".csv",sep=""),header=TRUE)
  prot$blx  <- NA
  prot$bly  <- NA
  prot$blok <- NA
  
  
  baseline <- numeric()  # Position (x/y)
  validfix <- numeric()  # Valid fixation time (ms), excluding 1st fixation
  code <- vpn
  print(code)
  
  ###########################################
  # 1. Determine baselines
  # Generate empty data field to store data
  
  # Determine trial number
  vpfix <- fixa[tolower(fixa$vp)==code,]
  ntrial <- nrow(prot)
  
  # Loop over trials to determine trial-by-trial baselines
  for (trial in 1:ntrial) {
    #trial = 1
    # Select trial data
    fixblock <- fixa[
      tolower(fixa$vp)==code & #tolower = translates characters in character vectors
        fixa$trial==trial,]
    msgblock <- msg [
      tolower(msg$vp)==code &
        msg$trial==trial,]
    
    # Check if correct Stimulus marker is present in MSG file
    if (msgblock$message!=paste(prot$pic[trial],
                               ".jpg",sep="")) {
       print(paste("Stimulus error: Trial:",trial," Event:",msgblock$message,sep=""))
     }
    
    # Determine onset (in ms)
    onset <- as.numeric(msgblock$time)
    
    # Subtract onset from timestamps
    fixblock$timest  <- as.numeric(fixblock$timest)-onset
    fixblock$timeend <- as.numeric(fixblock$timeend)-onset
    
    # Cut last fixation to presentation time
    fixblock$timeend[nrow(fixblock)] <- 
      ifelse(
        fixblock$timeend[
          nrow(fixblock)]>dur,
        dur,
        fixblock$timeend[
          nrow(fixblock)])
    
    
    # Calculate valid fixation time
    fixblock$dur <- as.numeric(fixblock$timeend) - as.numeric(fixblock$timest)
    validfix <- c(validfix,sum(fixblock$dur[fixblock$timest>0],na.rm=TRUE))
    
    # Calculate baseline as weighted average of fixations
    fixblockbl <- fixblock[fixblock$timeend>blst & fixblock$timest<blen,]
    if (nrow(fixblockbl)>0) {
      # Restrict fixation data to baseline
      fixblockbl$timest[1] <- ifelse(head(fixblockbl$timest,1)<blst,blst,head(fixblockbl$timest,1))
      fixblockbl$timeend[nrow(fixblockbl)] <- ifelse(tail(fixblockbl$timeend,1)>blen,blen,tail(fixblockbl$timeend,1))
      
      
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
  
  # Determine outlier
  blxok <- outlier_remove(baseline[,1])
  blyok <- outlier_remove(baseline[,2])
  # Baseline is valid when x and y coordinates are ok (i.e. no outlier)
  blok <- as.numeric((blxok==1) & (blyok==1))
  
  prot$blx <- baseline[,1]
  prot$bly <- baseline[,2]
  prot$blok <- blok
  prot$validfix <- validfix
  
  # Testplot
  if (plotBL) {
    plot(baseline[,1],baseline[,2],pch=16,col="black",xlab="x (px)",ylab="y (px)",xlim=c(0,1920),ylim=c(0,1080))
    points(baseline[blok==0,1],baseline[blok==0,2],pch=16,col="red")
    title(code)
  }
  
  # Store number of valid baselines per subject
  bl_quality_vp <- c(nrow(baseline),sum(blok))
  
  # Store spread of baselines in x and y direction
  xrng <- max(baseline[blok==1,1])-min(baseline[blok==1,1])
  yrng <- max(baseline[blok==1,2])-min(baseline[blok==1,2])
  bl_quality_vp <- c(bl_quality_vp,xrng,yrng)
  
  write.csv2(prot,paste(path.prot,vpcode,"_BL.csv",sep=""),row.names=FALSE,quote=FALSE)
  
  bl_quality <- rbind(bl_quality,bl_quality_vp)
  
  if (plotBL) {
    dev.off()
  }
}

erg <- data.frame(vps,bl_quality,row.names=NULL)
names(erg) <- c("code","alltrials","blok","xrng","yrng")

# Mark problematic cases
erg$pblok <- erg[,3]/erg[,2]
erg$problem  <- as.numeric((erg$xrng>150) | (erg$yrng>150))
write.csv2(erg,paste(path.eye,"Results_BaselineCheck.csv",sep=""),row.names=FALSE,quote=FALSE)

