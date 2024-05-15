################################################################################
# Project: Anticipatory saccades for prevention actions
# - Score baselines and determine baseline quality


## prepare workspace
rm(list = ls())
library(tidyverse)
source("z_outlier_remove.R")
source("z_get_baseline_x_y.R")


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
path_prot <- "Analyse/prot/" %>% paste0(path, .)
path_save <- "Analyse/BL/" %>% paste0(path, .) 


## set some variables for baseline correction
# Baseline between -300 and 0 ms relative to stimulus onset
baseline_start <- -300
baseline_end <- 0
# Stimulus duration
dur_max <- 10000


## load data for all participants
sac  <- read.csv2(paste0(path, "Eyelink/sacc.csv"), row.names = 1)
msg  <- read.csv2(paste0(path, "Eyelink/msg.csv"), row.names = 1) %>%
  filter(event == "Cue")
fixa <- read.csv2(paste0(path, "Eyelink/fixa.csv"), row.names = 1)


## pre-process fixations
fixa <- fixa %>%
  left_join(msg) %>%
  mutate_at(c("timest", "timeend", "time"), as.numeric) %>% 
  group_by(vp, trial) %>%
  mutate(
    block = ifelse(trial <= 100, 1, 2),
    start = timest - time,
    end = timeend - time,
    end = ifelse(end > dur_max, dur_max, end),                 # Cut last fixation to presentation time
    dur = end - start,                                         # Calculate valid fixation time
  )

trial_info <- fixa %>%
  group_by(vp, trial, block) %>%                                # block constant within trial, but do not loose this information
  summarize(
    valid_fix  = sum(ifelse(start > 0, dur, 0), na.rm = TRUE),  # Valid fixation time (ms), excluding 1st fixation
    baseline_x = get_baseline_x_y(baseline_start, baseline_end, start, end, x, y)$x,
    baseline_y = get_baseline_x_y(baseline_start, baseline_end, start, end, x, y)$y,
  ) %>% 
  group_by(vp, block) %>%
  mutate(
    baseline_x_ok   = outlier_remove(baseline_x),
    baseline_y_ok   = outlier_remove(baseline_y),
    baseline_ok_sd  = as.numeric((baseline_x_ok == 1) & (baseline_y_ok == 1)),
    baseline_x_mean = mean(baseline_x, na.rm = TRUE),                           
    baseline_y_mean = mean(baseline_y, na.rm = TRUE),
    baseline_ok_abs = ifelse(  
      abs(baseline_x_mean - baseline_x) > 50 | abs(baseline_y_mean - baseline_y) > 50, 
      0, 
      1),
    baseline_ok     = ifelse(baseline_ok_sd == 0 | baseline_ok_abs == 0, 0, 1)  
  )

  
## iterate over all participants and generate new files, separately for participants
# data frame to have an overview over data exclusions
baseline_quality <- data.frame()
data_frame_with_200_trials <- data.frame(trial = 1:200)


# participants that are omitted from processing
# no data recording for vp49 in the second half of the experiment in Exp. 2; skip this person in Exp. 2
if (exp == "exp_1"){
  skip_participants <- c()
} else if (exp == "exp_2") {
  skip_participants <- c("vp49")
}

# enumerate string to select participants to preprocess
subjects_to_analyze <- fixa %>%
  pull(vp) %>%
  unique() %>%
  as.character()


## iterate over all participants
for (curr_subject in subjects_to_analyze) {
  # skip the participants where data logging failed
  if (curr_subject %in% skip_participants) { next }
  
  ## read in subject data, create empty storages
  prot <- read.csv2(paste0(path_prot, curr_subject, ".csv"), header = TRUE)
  
  ## get subject trial info
  subject_trial_info <- trial_info %>%
    filter(vp == curr_subject) %>%
    rename(
      blx  = baseline_x,
      bly  = baseline_y,
      blok1 = baseline_ok_sd,          
      blok2 = baseline_ok_abs,
      blok = baseline_ok,
      validfix = valid_fix,
      blmeanx = baseline_x_mean,
      blmeany = baseline_y_mean
    ) %>%
    ungroup() %>%
    select(-block, -baseline_x_ok, -baseline_y_ok)
  
  # defensive programming is always a good idea - will fire for the current
  # project, but this is expected and ok
  if (nrow(subject_trial_info) != 200) {
    print(paste0(curr_subject, " has ", nrow(subject_trial_info), " trials of eyetracking information."))
    subject_trial_info <- data_frame_with_200_trials %>%
      left_join(subject_trial_info, by = join_by("trial")) %>%
      mutate(  
        vp = curr_subject,   # convert NA values
        )
  }
  
  subject_trial_info <- subject_trial_info %>%
    mutate(  
      blok1 = ifelse(is.na(blok1), 0, blok1),  # convert NA values
      blok2 = ifelse(is.na(blok2), 0, blok2),  
      blok  = ifelse(is.na(blok) , 0, blok),
    )
  
  prot <- prot %>% 
    left_join(
      subject_trial_info,
      by = join_by("trial", "vp")
    )
  
  ## write baselines to hard drive
  write.csv2(
    prot, paste(path_prot, curr_subject, "_BL.csv", sep = ""),
    row.names = FALSE, quote = FALSE
  )
  
  ## keep track over some aggregates for participant exclusion
  baseline_quality_vp <- data.frame(
    vp = curr_subject, 
    alltrials = nrow(prot), 
    blok = sum(prot$blok)
    )
  baseline_quality <- rbind(baseline_quality, baseline_quality_vp)
}


## print the data exclusion overview
baseline_quality <- baseline_quality %>% mutate(
  pblok = blok/alltrials, 
  prob = ifelse(pblok <= 0.5, 1, 0)
)
sum(baseline_quality$prob)

write.csv2(
  baseline_quality,
  paste(path_prot, "Results_BaselineCheck_blockwise.csv", sep = ""),
  row.names = FALSE,
  quote = FALSE
)

# Define cases with too many outliers (>50%) to exclude from further analyses
eye_invalid_baseline <- baseline_quality %>% 
  filter(prob == 1) %>% 
  pull(vp) %>% 
  unique()


