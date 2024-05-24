################################################################################
# Project: Anticipatory saccades for prevention actions
#

#---------------------------------------------------------
# SET ENVIRONMENT
rm(list=ls()) 
options(digits = 7)
library(tidyverse)

#Functions -----------------------------------------------
se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}
  
#---------------------------------------------------------
# SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[1]

### READ IN DATA
if (exp == "exp_1"){
  path <- ".\\Data\\Exp1\\"
} else if (exp == "exp_2") {
  path <- ".\\Data\\Exp2\\"
}

pupil_long <- read.csv2(paste(path,"Pupil/","Pupil_long.csv",sep=""))

#---------------------------------------------------------
### PLOT DATA

pupil.plot = pupil_long %>% group_by(OpPrev, ActPass, condition, time) %>% 
  summarise(mmChange.se = se(mmChange, na.rm=T), mmChange = mean(mmChange, na.rm=T))

print(pupil.plot %>% ggplot(aes(x=time, y=mmChange, group=condition)) + 
        geom_line(aes(colour = condition, linetype = condition), linewidth = 1) +
        geom_ribbon(aes(ymin=mmChange-mmChange.se, ymax=mmChange+mmChange.se, fill= condition), color =NA, alpha=.1) + 
        scale_linetype_manual(name="Trial type", values=c("solid", "dotdash", "solid", "dotdash"), labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive"))+
        scale_color_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive")) +
        scale_fill_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive")) +
        geom_line(aes(x = 0), color = "black") +
        annotate(geom = "text",
                 label = c("Picture Onset"),
                 x = c(10),
                 y = c(0),
                 angle = 90,
                 vjust = 1) +
        ylab("Pupil Size Change (mm)") + xlab("Time") + labs(color="condition") +
        theme_classic())

#---------------------------------------------------------
### ANALYSE DATA

pupil_long_test <- pupil_long %>%
  group_by(ActPass, OpPrev, vp)%>%
  dplyr::filter(time >= -600)%>%
  summarise(
    Anticipation = mean(mmChange, na.rm = TRUE)
  )

means_pupil_test <- pupil_long %>%
  group_by(ActPass, OpPrev)%>%
  dplyr::filter(time >= -600)%>%
  summarise(
    Anticipation = mean(mmChange, na.rm = TRUE)
  )

means_pupil_test_actpass <- pupil_long %>%
  group_by(ActPass)%>%
  dplyr::filter(time >= -600)%>%
  summarise(
    Anticipation = mean(mmChange, na.rm = TRUE)
  )

means_pupil_test_opprev <- pupil_long %>%
  group_by(OpPrev)%>%
  dplyr::filter(time >= -600)%>%
  summarise(
    Anticipation = mean(mmChange, na.rm = TRUE)
  )

ez::ezANOVA(data = pupil_long_test, dv = Anticipation, wid = vp, within = c(OpPrev, ActPass), detailed = TRUE) %>% schoRsch::anova_out() 
t.test(Anticipation ~ OpPrev, data= (pupil_long_test %>% dplyr::filter((OpPrev == "prevent" & ActPass == "passive")|(OpPrev == "operant" & ActPass == "passive"))), paired = T) %>% schoRsch::t_out()
t.test(Anticipation ~ OpPrev, data= (pupil_long_test %>% dplyr::filter((OpPrev == "prevent" & ActPass == "active")|(OpPrev == "operant" & ActPass == "active"))), paired = T) %>% schoRsch::t_out()
