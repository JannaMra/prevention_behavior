############################################################################
# ShockScanning_Flight fMRI project
# 
# General 
# Design:
# Shock (cue1) vs NoShock (cue2) vs Flight (cue0)
# Stimulation:
# 2s fixation cross -> 8s stimulus -> 6-8s fixation cross with shock or notif(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

requirePackage = function(name, load=T) {
  package = as.character(name)
  if (package %in% rownames(installed.packages()) == FALSE) install.packages(package)
  if (load) library(package, character.only=TRUE)
}

{ #install packages needed
  requirePackage("tidyverse", load=F)
  requirePackage("scales", load=F)
  requirePackage("cowplot", load=F)
  requirePackage("readxl", load=F)
  requirePackage("apaTables", load=F)
  requirePackage("schoRsch", load=F)
  requirePackage("apa", load=F)
  requirePackage("ez", load=F)
  requirePackage("TOSTER", load=F)
  requirePackage("psych", load=F)
}

library(tidyverse)

{ # Variables ---------------------------------------------------------------
  # a priori exclusions for all variables
  exclusions = c(
  ) %>% unique() %>% sort()
  
  trials.n = 200 #number of trials that shall be analyzed (if more trials, last ones will be taken)
  
  preStim = 2000
  trialEnd = 10000
  itiEnd =  #not theoretical but empirical bounds
  
  threatLevels.n = 3
  
  sample.rate = 60 #samples/second
  trial.duration = sample.rate * trialEnd / 1000 #seconds
  
  #startID = "CONDITION" #identifier for trial start messages
  expoID = "Stimulus " #identifier for exposition start messages
  stimExt = ".jpg"
  
  screen.height = 1080 #height of screen in pix
  screen.width  = 1920 # width of screen in pix
}


# Paths -------------------------------------------------------------------
getwd()
#setwd("C:/Users/jat41gk/Documents/Projekte/Prevention behavior/prevention_behavior/")
setwd('..')
path = getwd() %>% paste0("/")
  #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Auswertung/" #@work

#load behavioral data (logs)
path.eye = "Data/Eyelink/reports/" %>% paste0(path, .) #eye tracking data
#path.trigger = "Data/Tobii/trigger/" %>% paste0(path, .) #get condition 
path.prot = "Analyse/prot/" %>% paste0(path, .)


# Files -------------------------------------------------------------------
files.eye = list.files(path.eye, pattern=".*.txt")
#files.trigger = list.files(path.trigger, pattern=".*.dat")

{ # Functions ---------------------------------------------------------------
  se = function(x, na.rm = FALSE) {
    sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
  }
  
  correlation_out = function(coroutput) {
    names = coroutput$data.name %>% strsplit(" and ") %>% unlist()
    cat(paste0("r(", names[1], ", ", names[2], "): ", coroutput %>% apa::cor_apa(print=F)), "\n")
  }
  
  F_apa = function(x) {
    cat("F(", paste(x$parameter, collapse=", "), ") = ", 
        round(x$statistic, 2), 
        ", p ",
        ifelse(x$p.value < .001, "< .001",
               paste0("= ", round(x$p.value, 2))),
        "\n", sep="")
  }
  
  ez.ci = function(ez, conf.level = .95, sph.corr=T) {
    for (effect in ez$ANOVA$Effect) {
      index.sphericity = which(ez$`Sphericity Corrections`$Effect == effect)
      GGe = ifelse(sph.corr==F || length(index.sphericity)==0, 1, ez$`Sphericity Corrections`$GGe[index.sphericity])
      
      index.effect = which(ez$ANOVA$Effect == effect)
      ez$ANOVA %>% with(apaTables::get.ci.partial.eta.squared(F[index.effect], DFn[index.effect]*GGe, DFd[index.effect]*GGe, conf.level = conf.level)) %>% 
        sapply(round, digits=2) %>% 
        paste0(collapse=", ") %>% paste0(effect, ": ", round(conf.level*100), "% CI [", ., "]\n") %>% cat()
    }
  }
  
  lmer.ci = function(lmer, conf.level = .95, twotailed=T) {
    values = lmer %>% summary() %>% .$coefficients %>% .[, c("Estimate", "df")] %>% data.frame()
    effects = values %>% rownames()
    
    for (i in seq(effects)) {
      psych::r.con(values$Estimate[i], values$df[i], p = conf.level, twotailed=twotailed) %>% 
        round(digits=2) %>% 
        paste0(collapse=", ") %>% paste0(effects[i], ": ", round(conf.level*100), "% CI [", ., "]\n") %>% cat()
    }
  }
  
  color.gradient.discrete = function(color.low, color.high, n) {
    scales::seq_gradient_pal(low=color.low, high=color.high)(seq(0, 1, length.out = n))
  }
  
  gradient.analysis = function(x) {
    n = length(x)
    level = mean(x)
    diff = x[n] - x[1]
    lds = mean(x[c(1, n)]) - mean(x[2:(n-1)])
    result = c(lds, diff, level); names(result) = c("lds", "diff", "level")
    return(result)
  }
  
  lds = function(x, standardize=F) {
    n = length(x)
    lds = mean(x[c(1, n)]) - mean(x[2:(n-1)])
    
    if (standardize) lds = lds / abs(x[n] - x[1])
    return(lds)
  }
  
  pathToCode = function(path, path.sep="/", file.ext="\\.") {
    first = path %>% gregexpr(path.sep, .) %>% sapply(max) %>% {. + 1}
    last = path %>% gregexpr(file.ext, .) %>% sapply(max) %>% {. - 1}
    return(path %>% substring(first, last))
  }
  
  codeToNum = function(code) code %>% gsub("\\D+", "", .) %>% as.integer()
  
  read.phys = function(path) {
    read.delim(path, na.strings="", skip=9) %>% rename(EDA = "CH1", ECG = "CH2", Trigger = "CH28") %>% 
      filter(Trigger <= 2^8)
  }
  
  get.trigger.onsets = function(x) {
    return(x %>% diff() %>% {. > 0} %>% which() %>% {. + 1})
    
    # triggers.all = which(x != 0) #all trigger indices (including trains)
    # triggers = c(first(triggers.all), #take very first instance
    #              triggers.all[triggers.all != lag(triggers.all)+1]) #and every instance that is not the same as its precursor index + 1
    # return(triggers[!is.na(triggers)])
  }
  
  recode.triggers = function(x, triggercode=1) {
    recode = rep.int(0, times=length(x))
    recode[get.trigger.onsets(x)] = triggercode
    return(recode)
  }
  
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
}

# plotting ----------------------------------------------------------------
colors = color.gradient.discrete("blue", "red", n=threatLevels.n)
dodge.width = .6 #good for factors on x-axis
dodge = position_dodge(width=dodge.width)
dodge_half = position_dodge(width=dodge.width/2)

dodge_stai = position_dodge(width=1)

#ggplot general theme
#theme_set( #don't use theme_set because it has to be executed every session but myGgTheme can be saved in global environment
myGgTheme <- theme_bw() + theme(
  #aspect.ratio = 1,
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill="white", color="white"),
  legend.background = element_rect(fill="white", color="grey"),
  legend.key=element_rect(fill='white'),
  axis.text = element_text(color="black"),
  axis.ticks.x = element_line(color="black"),
  axis.line.x = element_line(color="black"),
  axis.line.y = element_line(color="black"),
  legend.text = element_text(size=14, color="black"),
  legend.title = element_text(size=14, color="black"),
  strip.text.x = element_text(size=12, color="black"),
  axis.text.x = element_text(size=16, color="black"),
  axis.text.y = element_text(size=16, color="black"),
  axis.title = element_text(size=16, color="black"))
#)

