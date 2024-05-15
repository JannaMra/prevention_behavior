################################################################################
# Project: Anticipatory saccades for prevention actions
# - change format of some logging files to a more tabular format


## prepare workspace
rm(list = ls())
library(tidyverse)


## SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[1]


## set paths here, paths are RELATIVE to the current working directory 
## (getwd() should return the folder where this script is saved)
if (exp == "exp_1"){
  path <- "Data/Exp1/"
} else if (exp == "exp_2") {
  path <- "Data/Exp2/"
} 
path_log <- "log/" %>% paste0(path, .) 
path_seq <- "sequences/" %>% paste0(path, .) 
path_save <- "Analyse/prot/" %>% paste0(path, .)


## iterate over all participants and generate new files, separately for participants
# data frame to have an overview over data exclusions
responses_summary <- data.frame()

# participants that are omitted from processing
# no data recording for vp49 in the second half of the experiment in Exp. 2; skip this person in Exp. 2
if (exp == "exp_1"){
  skip_participants <- c()
} else if (exp == "exp_2") {
  skip_participants <- c("vp49")
}

# enumerate string to select participants to preprocess
if (exp == "exp_1"){
  vpn <- stringr::str_pad(1:56, 2, pad = "0") %>% paste0("vp", .)
} else if (exp == "exp_2") {
  vpn <- stringr::str_pad(1:69, 2, pad = "0") %>% paste0("vp", .)
} 

## iterate over all participants
for (vp in vpn) {
  # skip the participants where data logging failed
  if (vp %in% skip_participants) { next }
  
  ## read in seqence data
  seqdat1 <- read.table(paste0(path_seq, vp, "_1.txt"),header=TRUE)
  seqdat2 <- read.table(paste0(path_seq, vp, "_2.txt"),header=TRUE)
  
  seqdat <- bind_rows(seqdat1, seqdat2)
  seqdat$vp <- vp
  seqdat$trial <- c(1:200)
  seqdat$response <- NA 
  seqdat$rt <- NA
  
  
  ## read in Log
  # skip first two lines, as they are metainformation
  logdat1  <- read.table(
    paste0(path_log, vp, "_1-PreventionBehavior.log"), 
    skip = 2, blank.lines.skip = TRUE,
    header = TRUE, sep = "\t", fill = TRUE) 
  logdat2  <- read.table(
    paste0(path_log, vp, "_2-PreventionBehavior.log"),
    skip = 2, blank.lines.skip = TRUE, 
    header = TRUE, sep = "\t", fill = TRUE)
  
  logdat <- bind_rows(logdat1, logdat2) 
  logdat <- logdat %>%
    mutate(Subject = tolower(Subject)) %>%
    filter(grepl(vp, Subject)) %>%
    filter(!grepl("subject", Subject))

  
  
  ## extract response information from log data and add to sequence data
  get_rt <- function(event, time) {
    response_indices = which(event == "Response")
    if (length(response_indices) == 0) {
      return(NA)
    }
    return(time[min(response_indices)])
  }
  
  resp_info <- logdat %>% 
    mutate(
      cue_number = cumsum(as.character(Code) == "cue")
    ) %>% 
    filter(cue_number > 0) %>% 
    group_by(cue_number) %>%
    summarize(
      response = sum(Event.Type == "Response"),
      rt = get_rt(event = Event.Type, time = TTime/10)
    )
  
  # defensive programming is always a good idea - will not fire for the current
  # project
  if(nrow(resp_info) != nrow(seqdat)) {
    stop("Extraction of response information seems to have gone wrong")
  }
  
  seqdat$response <- resp_info$response
  seqdat$rt <- resp_info$rt
  
  
  ## write prot file to hard drive
  write.csv2(seqdat, paste0(path_save,vp,".csv"), row.names=FALSE, quote=FALSE)
  
  
  ## keep track over some aggregates for participant exclusion
  participant_responses_summary <- seqdat %>% 
    mutate(
      valence = ifelse(valence == 1, "operant", "prevention"),
      operant = ifelse(operant == 1, "active", "passive")
    ) %>%
    group_by(vp, operant, valence) %>%
    summarise(
      n_trials = n(),
      n_no_resps = sum(response == 0),
      n_resps = sum(response == 1),
      .groups = "keep"
    )
  responses_summary <- bind_rows(responses_summary, participant_responses_summary)
}


## print the data exclusion overview
too_many_responses <- responses_summary %>%
  filter(operant == "passive") %>%
  group_by(vp, valence) %>%
  summarize(
    m_keypress = (sum(n_trials) - sum(n_no_resps)) / sum(n_trials)
  ) %>% 
  filter(m_keypress > 0.5)

too_few_responses <- responses_summary %>%
  filter(operant == "active") %>%
  group_by(vp, valence) %>%
  summarize(
    m_keypress = (sum(n_trials) - sum(n_no_resps)) / sum(n_trials)
  ) %>% 
  filter(m_keypress < 0.5)
  
subjects_bad_passive <- too_many_responses %>% pull(vp) %>% unique()
subjects_bad_active <- too_few_responses %>% pull(vp) %>% unique()

bad_subjects <- c(subjects_bad_active, subjects_bad_passive) %>% unique()





