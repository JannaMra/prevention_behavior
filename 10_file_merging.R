################################################################################
# Project: Anticipatory saccades for prevention actions
# - merge the data files, exclude participants


#---------------------------------------------------------
# SET ENVIRONMENT
rm(list=ls()) 
options(digits = 7)
library(tidyverse)
source("z_merge_files.R")


#---------------------------------------------------------
# SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[1]


#---------------------------------------------------------
### READ IN DATA
if (exp == "exp_1"){
  path <- ".\\Data\\Exp1\\"
} else if (exp == "exp_2") {
  path <- ".\\Data\\Exp2\\"
}

dat_msg <-  data.table::fread(paste0(path, "EyeLink\\msg.csv"),  sep = ";", dec = ",")
dat_fixa <- data.table::fread(paste0(path, "EyeLink\\fixa.csv"), sep = ";", dec = ",") 
dat_sacc <- data.table::fread(paste0(path, "EyeLink\\sacc.csv"), sep = ";", dec = ",")

# Directory of the counterbalancing files
dir <- paste0(path, "Analyse\\prot\\")
# Regex matching the data file names
filename_pattern <- "vp\\d{2,}\\_BL.csv"
# Files matching the pattern
file_list <- list.files(path = dir, pattern = filename_pattern, full.names = T, recursive = T, no.. = T)
# read in and merge
dat_seq <- merge_files(file_list, dec = ",") %>% 
  rename(
    OpPrev = valence, 
    ActPass = operant
  )


# Directory of the rating files
dir <- paste0(path, "Rating\\")
# Regex matching the data file names
filename_pattern <- "vp\\d{2,}\\_rating.csv"
# Files matching the pattern
file_list <- list.files(path = dir, pattern = filename_pattern, full.names = T, recursive = T, no.. = T)
# read in and merge
dat_rating <- merge_files(file_list, dec = ".")

dat_rating <- dat_rating %>% mutate(
  vp = str_extract(fileName, "vp\\d{2,}"),
  posneg = ifelse(str_detect(pic, "pos") == T, "positive", NA),
  posneg = ifelse(str_detect(pic, "neg") == T, "negative", posneg),
)


#---------------------------------------------------------
### PARTICIPANT EXCLUSION

# Exp. 1
# excluded due to too few responses
# vp10, vp11, vp12, vp15, vp17, vp27
# excluded due to invalid baseline
# vp12, vp34, vp51

# Exp. 2
# excluded due to too few responses
# vp != "vp05" & vp != "vp09" & vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp13" & vp != "vp17" & vp != "vp18" & vp != "vp19" & vp != "vp22" & vp != "vp23" & vp != "vp24" & vp != "vp28" & vp != "vp29" & vp != "vp32" & vp != "vp42" & vp != "vp49, vp66
# excluded due to invalid baseline
# vp63

if (exp == "exp_1"){
  bad_responses <-  c(10, 11, 12, 15, 17, 27)
  bad_baseline <- c(12, 34, 51)
} else if (exp == "exp_2") {
  bad_responses <- c(5, 8, 9, 10, 11, 12, 13, 16, 17, 18, 19, 22, 23, 24, 28, 29, 32, 42, 49, 66) 
  bad_baseline <- c(63) 
}

bad_responses <- bad_responses %>%
  str_pad(2, pad = "0") %>% 
  paste0("vp", .)

bad_baseline <- bad_baseline %>%
  str_pad(2, pad = "0") %>% 
  paste0("vp", .)

bad_subjects <- c(bad_responses, bad_baseline)

dat_msg    <- dat_msg    %>% filter(!(vp %in% bad_subjects)) %>% select(-V1)  # ignore row number
dat_fixa   <- dat_fixa   %>% filter(!(vp %in% bad_subjects)) %>% select(-V1)  # ignore row number
dat_sacc   <- dat_sacc   %>% filter(!(vp %in% bad_subjects)) %>% select(-V1)  # ignore row number
dat_seq    <- dat_seq    %>% filter(!(vp %in% bad_subjects))
dat_rating <- dat_rating %>% filter(!(vp %in% bad_subjects))


#---------------------------------------------------------
### SACCADES

dat_msgfilt <- dat_msg %>% filter(event != "Cue")
dat_sacc_filter <- full_join(dat_seq, dat_sacc, by = c("vp", "trial")) %>%
  full_join(dat_msgfilt, by = c("vp", "trial")) %>% 
  relocate(c(vp, trial, OpPrev, ActPass)) %>% 
  relocate(fileName, .after = event) %>% 
  mutate(
    OpPrev = ifelse(OpPrev == 1, "operant", "prevention"),
    ActPass = ifelse(ActPass == 1, "active", "passive")
  )
# data file with participant exclusions, but without trial exclusions
data.table::fwrite(dat_sacc_filter, paste0(path, "dat_sacc.csv"), sep = ",", dec = ".")  


#---------------------------------------------------------
### FIXATIONS
dat_fixa_filter <- full_join(dat_seq, dat_fixa, by = c("vp", "trial")) %>%
  full_join(dat_msgfilt, by = c("vp", "trial")) %>% 
  relocate(c(vp, trial, OpPrev, ActPass)) %>% 
  relocate(fileName, .after = event) %>%  
  mutate(
    OpPrev = ifelse(OpPrev == 1, "operant", "prevention"),
    ActPass = ifelse(ActPass == 1, "active", "passive")
  )
# data file with participant exclusions, but without trial exclusions
data.table::fwrite(dat_fixa_filter, paste0(path, "dat_fixa.csv"), sep = ",", dec = ".")  


#---------------------------------------------------------
### RATINGS
dat_rating <- dat_rating %>% mutate(
  posneg = case_when(
    str_detect(pic, "pos") ~ "positive",
    str_detect(pic, "neg") ~ "negative",
    .default = NA)
) %>%
  mutate(
    vp = str_extract(fileName, "vp\\d{2,}")
  ) %>% 
  relocate(vp, .before = nr) %>% 
  relocate(posneg, .after = nr)
data.table::fwrite(dat_rating, paste0(path, "dat_rating.csv"), sep = ",", dec = ".")








