############################################################################
# Prevention Behavior project
# 
# 

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
  trials.n = 200 #number of trials that shall be analyzed (if more trials, last ones will be taken)
  
  preStim = 2000
  trialEnd = 10000
  itiEnd =  #not theoretical but empirical bounds
  
  sample.rate = 60 #samples/second
  trial.duration = sample.rate * trialEnd / 1000 #seconds
  
  #startID = "CONDITION" #identifier for trial start messages
  expoID = "Stimulus " #identifier for exposition start messages
  stimExt = ".jpg"
  
  screen.height = 1080 #height of screen in pix
  screen.width  = 1920 # width of screen in pix
}


# Paths -------------------------------------------------------------------
path = #getwd() %>% paste0("/")
  #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Experiment 1/" #path Experiment 1 Janna
  #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Experiment 2/" #path Experiment 2 Janna



