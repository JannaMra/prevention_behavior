rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat_msg <- data.table::fread(".\\Data\\EyeLink\\msg.csv", sep = ";", dec = ",")
dat_fixa <- data.table::fread(".\\Data\\EyeLink\\fixa.csv", sep = ";", dec = ",")
dat_sacc <- data.table::fread(".\\Data\\EyeLink\\sacc.csv", sep = ";", dec = ",")


# excluded due to too less responses
# vp10, vp11, vp12, vp15, vp17, vp27
# excluded due to invalid baseline
# vp12, vp34, vp51

dat_msg <- dat_msg %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")
dat_fixa <- dat_fixa %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")
dat_sacc <- dat_sacc %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")


# merge counterbalance files
## Params:
# Directory of the data
#dir <- paste0(c(getwd(), ""), collapse = "")
dir <- ".\\Data\\Analyse\\prot\\"
# Regex matching the Tracking data file names
filename_pattern <- "vp\\d{2,}\\_BL.csv"


# <CODE>
file_list <- list.files(path = dir, pattern = filename_pattern, full.names = T, recursive = T, no.. = T)
dat_seq <- rainR::merge_files(file_list, dec = ",")
# </CODE>

dat_seq <- dat_seq %>% filter(vp != "vp10" & vp != "vp11" & vp != "vp12" & vp != "vp15" & vp != "vp17" & vp != "vp27" & vp != "vp34" & vp != "vp51")

dat_seq <- dat_seq %>% rename(OpPrev = valence, ActPass = operant)

dat_msgfilt <- dat_msg %>% filter(event != "Cue")

dat <- full_join(dat_seq, dat_sacc, by = c("vp", "trial"))
dat <- full_join(dat, dat_msgfilt, by = c("vp", "trial"))

dat <- dat %>% relocate(c(vp, trial, OpPrev, ActPass))
dat <- dat %>% relocate(c(fileName, path), .after = event)

dat <- dat %>% mutate(
  OpPrev = ifelse(OpPrev == 1, "operant", "prevention"),
  ActPass = ifelse(ActPass == 1, "active", "passive")
)


data.table::fwrite(dat, "dat.csv", sep = ",", dec = ".")

##########
#Fuer Fixationen anderes Datenfile
rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat_msg <- data.table::fread(".\\Data\\EyeLink\\msg.csv", sep = ";", dec = ",")
dat_fixa <- data.table::fread(".\\Data\\EyeLink\\fixa.csv", sep = ";", dec = ",")
dat_sacc <- data.table::fread(".\\Data\\EyeLink\\sacc.csv", sep = ";", dec = ",")


# excluded due to too less responses
# vp10, vp11, vp12, vp15, vp17, vp27
# excluded due to invalid baseline
# vp12, vp34, vp51

bad_subjects <- c("vp10", "vp11", "vp12", "vp15", "vp17", "vp27", "vp34", "vp51")

dat_msg <- dat_msg %>% filter(!(vp %in% bad_subjects))
dat_fixa <- dat_fixa %>% filter(!(vp %in% bad_subjects))
dat_sacc <- dat_sacc %>% filter(!(vp %in% bad_subjects))


# merge counterbalance files
## Params:
# Directory of the data
#dir <- paste0(c(getwd(), ""), collapse = "")
dir <- ".\\Data\\Analyse\\prot\\"
# Regex matching the Tracking data file names
filename_pattern <- "vp\\d{2,}\\_BL.csv"


# <CODE>
file_list <- list.files(path = dir, pattern = filename_pattern, full.names = T, recursive = T, no.. = T)
dat_seq <- rainR::merge_files(file_list, dec = ",")
# </CODE>

dat_seq <- dat_seq %>% filter(!(vp %in% bad_subjects))

dat_seq <- dat_seq %>% rename(OpPrev = valence, ActPass = operant)

dat_msgfilt <- dat_msg %>% filter(event != "Cue")

dat <- full_join(dat_seq, dat_fixa, by = c("vp", "trial"))
dat <- full_join(dat, dat_msgfilt, by = c("vp", "trial"))

dat <- dat %>% relocate(c(vp, trial, OpPrev, ActPass))
dat <- dat %>% relocate(c(fileName, path), .after = event)

dat <- dat %>% mutate(
  OpPrev = ifelse(OpPrev == 1, "operant", "prevention"),
  ActPass = ifelse(ActPass == 1, "active", "passive")
)

data.table::fwrite(dat, "dat2.csv", sep = ",", dec = ".")
