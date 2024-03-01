##################################################################
# Shockscanning Flight Choice
# Projekt 
#
# Calculate fixation density maps for each condition and observer
#
# Cue-Kodierung: 0 = Flight, 1 = Shock, 2 = Safety
# changed here for order into 0 = Shock, 1 = Flight, 2 = Safety

#rm(list=ls())

require("spatstat")
#require("readbitmap")
#require("jpeg")
#require("png")

st <- -600; en <- 0  # Scoring range

# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -900; blen <- -600

# Image dimensions: Würzburg 
# screen resolution: 1920 x 1200 pixels
screen.height = 1200 #height of screen in pix
screen.width  = 1920 # width of screen in pix

wsx <- 1920 ; wsy <- 1200
# Image size: 768 x 576 pixels (visual angle of 24.07 x 16.3)

# Image dimensions: Trier 
# screen resolution: 1920 x 1200 pixels
screen.height = 1080 #height of screen in pix
screen.width  = 1920 # width of screen in pix

wx <- 1920; wy <- 1080
# Image size: 768 x 576 pixels (visual angle of 24.07 x 16.3)



# Wie viele Pixel result in a visual angle of 1?
# Distance from monitor: 500 mm
# Monitor: BOLDscreen 32 LCD for fMRI
# 1920 x 1080, RGB colour with fixed 120Hz frame rate
# Display size 698.4mm x 392.9mm 
# Resolution: 1920 x 1080 Pixel
# pic size: 768 × 576 Pixel
visdeg <- 40   


protpath <- "Data/Analyse/prot/" %>% paste0(path, .) # Würzburg Data
savepath <- "Data/Plots/Data/" %>% paste0(path, .)

# Eyetrackingdaten laden 
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1) %>% filter(event != "Cue")
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

# Exclusions:
print(eye.invalid.bl)

# Determine which subjects should be analyzed
#vpn <- c(paste("tfo",ifelse(1:16<10,"0",""),1:16,sep=""))
vpn = fixa$vp %>% unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
vpn <-  vpn[!(vpn %in% eye.invalid.bl)]
vpn <-  vpn[!(vpn %in% exclusion.responses)]
vpn.n = length(vpn)


# Loop across participants and save individual fixation density maps for each subject
fixinval <- numeric()
#for (vp in vpn) {
for (vp in vpn) {
  #vp <- "vp21"
  print(vp)
  # read in data
  vpcode <- vp #paste0(ifelse(vpn<10, paste(c("0", vpn), collapse = ""),vpn))    #ifelse(vp<10,0,""),vp)
  print(vpcode)
  
  prot <- read.csv2(paste(protpath,vpcode,"_BL.csv",sep=""))%>%
    mutate(condition = case_when(valence == 1 & operant == 1 ~ 1, # 1 = active operant
                           valence == 1 & operant == 0 ~ 2,       # 2 = passive operant
                           valence == 0 & operant == 1 ~ 3,       # 3 = active prevent
                           valence == 0 & operant == 0 ~ 4))       # 4 = passive prevent
  
  fixdens <- list()
  fixinvalvp <- numeric()
  
  # loop through different cue types
  for (condition in c(1,2,3,4)) {
    if (exists("fixall")) { rm("fixall") }
    for (trialnr in 1:nrow(prot)) {  
      #trialnr <- 151
      # only analyse when Baseline OK and cue available
      if ((prot$blok[trialnr]== 1) & prot$condition[trialnr]==condition) {
        fixblock <- fixa[fixa$vp==vp & fixa$trial==trialnr,]
        
        # define onset
        onset  <- msg$time[msg$vp==vp & msg$trial==trialnr]

        if (nrow(fixblock)>0) {
          # shift to onset
          fixblock$timest  <- fixblock$timest-onset # time of fixcross and first half of trial
          fixblock$timeend <- fixblock$timeend-onset # time of fixcross
          
          # consider relevant fixations only
          fixblock <- fixblock[fixblock$timeend>st & fixblock$timest<en,] # switched start and end here ?
          fixblock <- fixblock %>% 
            mutate(timest = case_when(timest < st ~ st,
                                      timest >= st ~ timest),
                   timeend = case_when(timeend > en ~ en,
                                       timeend <= en ~ timeend))
            
          
          if (nrow(fixblock)>0) { 
            # correct fixations for baseline
            fixblock$x <- fixblock$x-prot$blx[trialnr]
            fixblock$y <- fixblock$y-prot$bly[trialnr]+150
            
            if (!exists("fixall")) {
              fixall <- fixblock
            } else {
              fixall <- rbind(fixall, fixblock)
            }
          }
        } 
      }
    }
    
    fixall$dur <- fixall$timeend - fixall$timest
    
    # Distance from center of display
    fixall$dist <- sqrt(fixall$x^2+fixall$y^2)
    
    # Valid sample (i.e. on screen?)
    fixall$valid <- 0
    fixall$valid[(fixall$x>(-wsx/2)) & (fixall$x<(wsx/2)) &
                 (fixall$y>(-wsy/2)) & (fixall$y<(wsy/2))] <- 1
    
    fixinvalvp <- c(fixinvalvp, sum(fixall$valid==0))
    
    # Build data vectors with 1ms sample rows
    xsmp <- rep(fixall$x,fixall$dur)
    ysmp <- rep(fixall$y,fixall$dur)
    
    # Calculate fixation density map
    X <- suppressWarnings(ppp(xsmp+wsx/2, (-1)*(ysmp-wsy/2), c(0,wsx), c(0,wsy)))
    #suppressWarnings(plot(X))
    Y <- density(X, sigma=visdeg, dimyx=c(wsy,wsx)) # from spatsat: sigma(1SD)=visdeg -> M-1SD & M+1SD = 2*visdeg
    # Normalize image
    Yden <- (Y$v-min(Y$v))/(max(Y$v)-min(Y$v))
    # Flip rows
    Yden <- Yden[nrow(Yden):1,]
    
    fixdens[[condition]] <- Yden
  } # end cue loop
  
  fixinval <- rbind(fixinval,fixinvalvp)
  save(fixdens, file=paste(savepath, vpcode, ".RData", sep=""))
} # end of trial loop
  

# Same for picture onset period ------------------------------------------------

require("spatstat")
#require("readbitmap")
#require("jpeg")
#require("png")

st <- 0; en <- 2700  # Scoring range

# Baseline between -300 and 0 ms relative to stimulus onset
blst <- -300; blen <- 0

# Image dimensions: Würzburg 
# screen resolution: 1920 x 1200 pixels
screen.height = 1200 #height of screen in pix
screen.width  = 1920 # width of screen in pix

wsx <- 1920 ; wsy <- 1200
# Image size: 768 x 576 pixels (visual angle of 24.07 x 16.3)

# Image dimensions: Trier 
#screen resolution: 1920 x 1200 pixels
screen.height = 1080 #height of screen in pix
screen.width  = 1920 # width of screen in pix

wx <- 1920; wy <- 1080
# Image size: 768 x 576 pixels (visual angle of 24.07 x 16.3)



# Wie viele Pixel result in a visual angle of 1?
# Distance from monitor: 500 mm
# Monitor: BOLDscreen 32 LCD for fMRI
# 1920 x 1080, RGB colour with fixed 120Hz frame rate
# Display size 698.4mm x 392.9mm 
# Resolution: 1920 x 1080 Pixel
# pic size: 768 × 576 Pixel
visdeg <- 40   


protpath <- "Data/Analyse/prot/" %>% paste0(path, .) # Würzburg Data
savepath <- "Data/Plots/Data/PictureOnset/" %>% paste0(path, .)

# Eyetrackingdaten laden 
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1) %>% filter(event != "Cue")
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

# Exclusions:
print(eye.invalid.bl)

# Determine which subjects should be analyzed
#vpn <- c(paste("tfo",ifelse(1:16<10,"0",""),1:16,sep=""))
vpn = fixa$vp %>% unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
vpn <-  vpn[!(vpn %in% eye.invalid.bl)]
vpn <-  vpn[!(vpn %in% exclusion.responses)]
vpn.n = length(vpn)


# Loop across participants and save individual fixation density maps for each subject
fixinval <- numeric()
#for (vp in vpn) {
for (vp in vpn) {
  #vp <- "vp21"
  print(vp)
  # read in data
  vpcode <- vp #paste0(ifelse(vpn<10, paste(c("0", vpn), collapse = ""),vpn))    #ifelse(vp<10,0,""),vp)
  print(vpcode)
  
  prot <- read.csv2(paste(protpath,vpcode,"_BL.csv",sep=""))%>%
    mutate(condition = case_when(valence == 1 & operant == 1 ~ 1, # 1 = active operant
                                 valence == 1 & operant == 0 ~ 2,       # 2 = passive operant
                                 valence == 0 & operant == 1 ~ 3,       # 3 = active prevent
                                 valence == 0 & operant == 0 ~ 4))       # 4 = passive prevent
  
  fixdens <- list()
  fixinvalvp <- numeric()
  
  # loop through different cue types
  for (condition in c(1,2,3,4)) {
    if (exists("fixall")) { rm("fixall") }
    for (trialnr in 1:nrow(prot)) {  
      #trialnr <- 151
      # only analyse when Baseline OK and cue available
      if ((prot$blok[trialnr]== 1) & prot$condition[trialnr]==condition) {
        fixblock <- fixa[fixa$vp==vp & fixa$trial==trialnr,]
        
        # define onset
        onset  <- msg$time[msg$vp==vp & msg$trial==trialnr]
        
        if (nrow(fixblock)>0) {
          # shift to onset
          fixblock$timest  <- fixblock$timest-onset # time of fixcross and first half of trial
          fixblock$timeend <- fixblock$timeend-onset # time of fixcross
          
          # consider relevant fixations only
          fixblock <- fixblock[fixblock$timeend>st & fixblock$timest<en,] # switched start and end here ?
          fixblock <- fixblock %>% 
            mutate(timest = case_when(timest < st ~ st,
                                      timest >= st ~ timest),
                   timeend = case_when(timeend > en ~ en,
                                       timeend <= en ~ timeend))
          
          
          if (nrow(fixblock)>0) { 
            # correct fixations for baseline
            fixblock$x <- fixblock$x-prot$blx[trialnr]
            fixblock$y <- fixblock$y-prot$bly[trialnr]+150
            
            if (!exists("fixall")) {
              fixall <- fixblock
            } else {
              fixall <- rbind(fixall, fixblock)
            }
          }
        } 
      }
    }
    
    fixall$dur <- fixall$timeend - fixall$timest
    
    # Distance from center of display
    fixall$dist <- sqrt(fixall$x^2+fixall$y^2)
    
    # Valid sample (i.e. on screen?)
    fixall$valid <- 0
    fixall$valid[(fixall$x>(-wsx/2)) & (fixall$x<(wsx/2)) &
                   (fixall$y>(-wsy/2)) & (fixall$y<(wsy/2))] <- 1
    
    fixinvalvp <- c(fixinvalvp, sum(fixall$valid==0))
    
    # Build data vectors with 1ms sample rows
    xsmp <- rep(fixall$x,fixall$dur)
    ysmp <- rep(fixall$y,fixall$dur)
    
    # Calculate fixation density map
    X <- suppressWarnings(ppp(xsmp+wsx/2, (-1)*(ysmp-wsy/2), c(0,wsx), c(0,wsy)))
    #suppressWarnings(plot(X))
    Y <- density(X, sigma=visdeg, dimyx=c(wsy,wsx)) # from spatsat: sigma(1SD)=visdeg -> M-1SD & M+1SD = 2*visdeg
    # Normalize image
    Yden <- (Y$v-min(Y$v))/(max(Y$v)-min(Y$v))
    # Flip rows
    Yden <- Yden[nrow(Yden):1,]
    
    fixdens[[condition]] <- Yden
  } # end cue loop
  
  fixinval <- rbind(fixinval,fixinvalvp)
  save(fixdens, file=paste(savepath, vpcode, "_PictureOnset.RData", sep=""))
} # end of trial loop

