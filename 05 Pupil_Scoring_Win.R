###############################################################################
# Prevention Behavior project
# Solveig Tonn & Janna Teigeler
#
# Extract pupil responses from EDF-files:
# - Trial segmentation
# - Downsampling
# - Low-pass filtering
# - Conversion to mm
# - Save single-trial data

require("eyelinkReader")
#require("signal") #read in functions with signal::, otherwise filter()function from dplyr does not work anymore

options(warn=0)

#rm(list=ls())

basepath <-  paste0(path,"Data/EyeLink/") #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Data/EyeLink/" #"C:/Users/Public/Documents/Local/Projekte/Pupiltest/"
savepath <- paste0(path,"Data/Pupil/") #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Data/Pupil/" #"C:/Users/Public/Documents/Local/Projekte/Pupiltest/"

#vpn <- c("Look002")

hz <- 500
newhz <- 100

smoothing <- TRUE    # Smooth time series (2 Hz)?
conversion <- TRUE   # Convert to mm according to Hayes & Petrov (2016)

# Distance Eye - Screen Center (mm)
distance <- 590 #Experiment 1
#distance <- 610 #Experiment 2

# Scoring-Window
st <- -1000; en <- 4000  # Prestimulus-Baseline start and end

# Onsets laden
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1)
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

# get VPn
#vpn.eye = fixations$subject %>% unique() %>% setdiff(exclusions.eye.num) %>% sort()
vpn.eye = fixa$vp %>% 
  unique() %>% sort() %>% as.character()
vpn <- vpn.eye %>% setdiff(eye.invalid.bl) %>% setdiff(exclusion.responses) #vpdat$filename

ga <- numeric()

# Loop over subjects
for (vp in vpn) {
  #vp <- "vp69"
  code <- vp
  print(code)
  prot_all <- data.frame()
  
  for (block in 1:2) {
    # Load eye-tracking data
    #block <- 3
  edfdat <- read_edf(paste0(basepath,code,"_", block,".edf"), import_samples=TRUE)
  
  # Detect blinks
  edfdat$samples$blink <- 0
  for (i in 1:nrow(edfdat$blinks)) {
    # Find saccade that belongs to the blink (to include full blink period)
    sacnr <- (1:nrow(edfdat$saccades))[(edfdat$saccades$entime>edfdat$blink$sttime[i]) & (edfdat$saccades$sttime<edfdat$blink$entime[i])]
    if (length(sacnr)==0) {  # no saccace found -> use blink times
      blinkst <- edfdat$blinks$sttime[i]
      blinken <- edfdat$blinks$entime[i]
    } else {
      blinkst <- edfdat$saccades$sttime[sacnr]
      blinken <- edfdat$saccades$entime[sacnr]
      # If corresponding saccades is shorter than blink (can happen at trial end) -> correct to blink times
      if (blinkst>edfdat$blinks$sttime[i]) {
        blinkst <- edfdat$blinks$sttime[i]
      }
      if (blinken<edfdat$blinks$entime[i]) {
        blinken <- edfdat$blinks$entime[i]
      }
    }
    edfdat$samples$blink[(edfdat$samples$time>=blinkst) & (edfdat$samples$time<=blinken)] <- 1
  }
  
  # create progress bar
  pb <- txtProgressBar(min=0, max=max(edfdat$samples$trial), style=3)
  
  ntrial <- length(unique(edfdat$samples$trial))
  
  # Throw out error when incorrect nr of trials
  if(block ==1 & ntrial!=100) { print(paste("Incorrect number of trials:",ntrial)) }
  if(block == 2 & ntrial!=100) { print(paste("Incorrect number of trials:",ntrial)) }
  
  # Get onsets 
  msgvp <- edfdat$events[grep("Cue",edfdat$events$message),]

  # Loop over trials to determine trial-by-trial pupil width
  rawpd <- numeric()
  for (trial in 1:ntrial) {
    # Set progress bar
    setTxtProgressBar(pb, trial)
    
    # Determine onset (in ms)
    onset <- msgvp$sttime[trial]
    
    # Get pupil data
    pupildat <- edfdat$samples[(edfdat$samples$trial==trial) &
                               (edfdat$samples$time>(onset+st)) & (edfdat$samples$time<=(onset+en)),]
    
    if (code=="") {   #left eye was tracked for this subject 
      pd <- pupildat$paL
    } else {
      pd <- pupildat$paR
    }
    
    # Only proceed when trial data are available
    if ((sum(!is.na(pd))>1) & (length(pd)>0)) {
      # Interpolate blinks
      pd[pupildat$blink==1] <- NA
      
      if (sum(!is.na(pd))>=2) {
        pdi <- approx(pupildat$time,pd,pupildat$time)
      } else {
        pdi <- list()
        pdi$x <- pupildat$time
        pdi$y <- pd
      }
      
      # Downsampling
      timelinenew <- seq(head(pdi$x,1)+((1000/newhz)-1)/2,tail(pdi$x,1),(1000/newhz))
      # Check if number of samples is sufficient / otherwise add timestamps
      while (length(timelinenew)<((en-st)/(1000/newhz))) {
        timelinenew <- c(timelinenew,tail(timelinenew,1)+(1000/newhz))
      }
      pdids <- signal::interp1(pdi$x, pdi$y, timelinenew)

      # Smooting?
      if (smoothing) {
        # Replicate first and last value to reduce filter artifacts
        pdidsf <- c(rep(pdids[1],100),pdids,rep(pdids[length(pdids)],100))
        pdidsf[is.na(pdidsf)] <- mean(pdidsf,na.rm=TRUE)
        bf <- signal::butter(2, 1/(newhz/2)*2)     # 2 Hz low-pass filter
        pdidssmooth <- signal::filtfilt(bf, pdidsf) # apply filter
        pdidssmooth <- pdidssmooth[101:(length(pdidssmooth)-100)]
      } else {
        pdidssmooth <- pdids
      }
      
      # Conversion to mm?
      # Based on Hayes & Petrov (2016)
      if (conversion) {
        # When DIAMETER instead of AREA was recorded -> Convert values
        if (edfdat$headers$rec_pupil_type[1]!="AREA") {
          pdidssmooth <- ((pdidssmooth/256)^2)*pi
        }
        
        pdidssmooth[pdidssmooth<0] <- 0
        pdidssmooth <- 1.70*10^(-4)*distance*sqrt(pdidssmooth)
      }
      
      # Store data
      rawpd <- rbind(rawpd,pdidssmooth)
      
    } else {
      rawpd <- rbind(rawpd,rep(NA,(en-st)/hz*newhz))
    }
  }
 
  close(pb)
  
  # Calculate grand average across trials
  ga <- rbind(ga, apply(rawpd,2,mean,na.rm=TRUE))
  
  # Save proprocessed data
  prot <- data.frame(trial=1:ntrial,rawpd)
  names(prot) <- c("trial",paste("pd",1:(ncol(rawpd)),sep=""))
  #write.csv2(prot,paste0(savepath,vp,"_pupil.csv"),row.names=FALSE,quote=FALSE)
  prot_all <- rbind(prot_all, prot)
  }
  
  prot_all$trial <- 1:(nrow(prot_all))
  write.csv2(prot_all,paste0(savepath,code,"_pupil.csv"),row.names=FALSE,quote=FALSE)
}

options(warn=0)

# Plot single participants
require(ggplot2)

plotdat <- data.frame(subj=rep(1:nrow(ga),each=ncol(ga)),
                      sec=seq(st/1000,en/1000-1/newhz,1/newhz),
                      y=as.numeric(t(ga)))

ggplot(plotdat, aes(y=y, x=sec, group=subj)) +
  geom_line(aes(color=subj)) +
  labs(x = "Time (s)") +
  labs(y = "Pupil change (mm)")

ggsave(paste(savepath,"Pupil-Changes_SingleSubj.png",sep=""),width=12,height=9,units="cm",scale=1.2)

# Plot grand averages
y    <- apply(ga,2,mean,na.rm=TRUE)
se   <- apply(ga,2,sd,na.rm=TRUE)/sqrt(nrow(ga))
sec  <- seq(st/1000,en/1000-1/newhz,1/newhz)
plotdat <- data.frame(sec,y,se)

ggplot(plotdat, aes(y=y, x=sec)) +
  geom_line() +
  geom_ribbon(data=plotdat,aes(ymin=y-se,ymax=y+se),alpha=0.3) +
  labs(x = "Time (s)") +
  labs(y = "Pupil change (mm)")

ggsave(paste(savepath,"Pupil-Changes_GAst0.png",sep=""),width=12,height=9,units="cm",scale=1.2)


# Extract Data only for Anticipation Period ----------------------------------------------------------------------------------------------------------------------

# Scoring-Window
st <- -1600; en <- 0  # Prestimulus-Baseline start and end

# get VPn
#vpn.eye = fixations$subject %>% unique() %>% setdiff(exclusions.eye.num) %>% sort()
vpn.eye = fixa$vp %>% 
  unique() %>% sort() %>% as.character()
vpn <- vpn.eye %>% setdiff(eye.invalid.bl) %>% setdiff(exclusion.responses) #vpdat$filename

# Onsets laden
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1)
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

ga <- numeric()

# Loop over subjects
for (vp in vpn) {
  #vp <- 54
  code <- vp
  print(code)
  prot_all <- data.frame()
  
  for (block in 1:2) {
    # Load eye-tracking data
    #block <- 3
    edfdat <- read_edf(paste0(basepath,code,"_", block,".edf"), import_samples=TRUE)
    
    # Detect blinks
    edfdat$samples$blink <- 0
    for (i in 1:nrow(edfdat$blinks)) {
      # Find saccade that belongs to the blink (to include full blink period)
      sacnr <- (1:nrow(edfdat$saccades))[(edfdat$saccades$entime>edfdat$blink$sttime[i]) & (edfdat$saccades$sttime<edfdat$blink$entime[i])]
      if (length(sacnr)==0) {  # no saccace found -> use blink times
        blinkst <- edfdat$blinks$sttime[i]
        blinken <- edfdat$blinks$entime[i]
      } else {
        blinkst <- edfdat$saccades$sttime[sacnr]
        blinken <- edfdat$saccades$entime[sacnr]
        # If corresponding saccades is shorter than blink (can happen at trial end) -> correct to blink times
        if (blinkst>edfdat$blinks$sttime[i]) {
          blinkst <- edfdat$blinks$sttime[i]
        }
        if (blinken<edfdat$blinks$entime[i]) {
          blinken <- edfdat$blinks$entime[i]
        }
      }
      edfdat$samples$blink[(edfdat$samples$time>=blinkst) & (edfdat$samples$time<=blinken)] <- 1
    }
    
    # create progress bar
    pb <- txtProgressBar(min=0, max=max(edfdat$samples$trial), style=3)
    
    ntrial <- length(unique(edfdat$samples$trial))
    
    # Throw out error when incorrect nr of trials
    if(block ==1 & ntrial!=100) { print(paste("Incorrect number of trials:",ntrial)) }
    if(block == 2 & ntrial!=100) { print(paste("Incorrect number of trials:",ntrial)) }
    
    # Get onsets 
    msgvp <- edfdat$events[grep("Stimulus",edfdat$events$message),]
    
    # Loop over trials to determine trial-by-trial pupil width
    rawpd <- numeric()
    for (trial in 1:ntrial) {
      # Set progress bar
      setTxtProgressBar(pb, trial)
      
      # Determine onset (in ms)
      onset <- msgvp$sttime[trial]
      
      # Get pupil data
      pupildat <- edfdat$samples[(edfdat$samples$trial==trial) &
                                   (edfdat$samples$time>(onset+st)) & (edfdat$samples$time<=(onset+en)),]
      
      if (code=="") {   #left eye was tracked for this subject 
        pd <- pupildat$paL
      } else {
        pd <- pupildat$paR
      }
      
      # Only proceed when trial data are available
      if ((sum(!is.na(pd))>1) & (length(pd)>0)) {
        # Interpolate blinks
        pd[pupildat$blink==1] <- NA
        
        if (sum(!is.na(pd))>=2) {
          pdi <- approx(pupildat$time,pd,pupildat$time)
        } else {
          pdi <- list()
          pdi$x <- pupildat$time
          pdi$y <- pd
        }
        
        # Downsampling
        timelinenew <- seq(head(pdi$x,1)+((1000/newhz)-1)/2,tail(pdi$x,1),(1000/newhz))
        # Check if number of samples is sufficient / otherwise add timestamps
        while (length(timelinenew)<((en-st)/(1000/newhz))) {
          timelinenew <- c(timelinenew,tail(timelinenew,1)+(1000/newhz))
        }
        pdids <- signal::interp1(pdi$x, pdi$y, timelinenew)
        
        # Smooting?
        if (smoothing) {
          # Replicate first and last value to reduce filter artifacts
          pdidsf <- c(rep(pdids[1],100),pdids,rep(pdids[length(pdids)],100))
          pdidsf[is.na(pdidsf)] <- mean(pdidsf,na.rm=TRUE)
          bf <- signal::butter(2, 1/(newhz/2)*2)     # 2 Hz low-pass filter
          pdidssmooth <- signal::filtfilt(bf, pdidsf) # apply filter
          pdidssmooth <- pdidssmooth[101:(length(pdidssmooth)-100)]
        } else {
          pdidssmooth <- pdids
        }
        
        # Conversion to mm?
        # Based on Hayes & Petrov (2016)
        if (conversion) {
          # When DIAMETER instead of AREA was recorded -> Convert values
          if (edfdat$headers$rec_pupil_type[1]!="AREA") {
            pdidssmooth <- ((pdidssmooth/256)^2)*pi
          }
          
          pdidssmooth[pdidssmooth<0] <- 0
          pdidssmooth <- 1.70*10^(-4)*distance*sqrt(pdidssmooth)
        }
        
        # Store data
        rawpd <- rbind(rawpd,pdidssmooth)
        
      } else {
        rawpd <- rbind(rawpd,rep(NA,(en-st)/hz*newhz))
      }
    }
    
    close(pb)
    
    # Calculate grand average across trials
    ga <- rbind(ga, apply(rawpd,2,mean,na.rm=TRUE))
    
    # Save proprocessed data
    prot <- data.frame(trial=1:ntrial,rawpd)
    names(prot) <- c("trial",paste("pd",1:(ncol(rawpd)),sep=""))
    #write.csv2(prot,paste0(savepath,vp,"_pupil.csv"),row.names=FALSE,quote=FALSE)
    prot_all <- rbind(prot_all, prot)
  }
  
  prot_all$trial <- 1:(nrow(prot_all))
  write.csv2(prot_all,paste0(savepath,code,"_pupil_anticipation.csv"),row.names=FALSE,quote=FALSE)
}

