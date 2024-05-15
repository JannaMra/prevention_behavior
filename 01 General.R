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
  "C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Experiment 1/" #path Experiment 1 Janna
  #"C:/Users/jat41gk/Documents/Projekte/Prevention behavior/Experiment 2/" #path Experiment 2 Janna

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
    
  }
  
  recode.triggers = function(x, triggercode=1) {
    recode = rep.int(0, times=length(x))
    recode[get.trigger.onsets(x)] = triggercode
    return(recode)
  }
  
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
}

## Experiment 1 -----------
#exclusion.responses <- c("vp10", "vp11", "vp12", "vp15", "vp17", "vp27")
#eye.invalid.bl <- c("vp12", "vp34", "vp51")

## Experiment 2 -----------

#exclusion.responses <- c("vp05", "vp08", "vp09", "vp10", "vp11", "vp12", "vp13", "vp16", "vp17", "vp18", "vp19", "vp22", "vp23", "vp24", "vp28", "vp29", "vp32", "vp42", "vp66")
#eye.invalid.bl <- c( "vp49", "vp63")

