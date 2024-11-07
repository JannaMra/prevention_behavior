###############################################################################
# Prevention Behavior project
# Solveig Tonn & Janna Teigeler
#
library(tidyverse) 

# Onsets laden
msg <- read.csv2(paste0(path,"Data/Eyelink/msg.csv"), row.names = 1)
fixa <- read.csv2(paste0(path,"Data/Eyelink/fixa.csv"), row.names = 1)
sac <- read.csv2(paste0(path,"Data/Eyelink/sacc.csv"), row.names = 1)

# Determine which subjects should be analyzed
vpn = fixa$vp %>% 
  unique() %>% sort() %>% as.character() #all subjects in fixations
vpn.n = length(vpn)
vpn = vpn[vpn %in% exclusions == F] #minus a priori exclusions
vpn <-  vpn[!(vpn %in% eye.invalid.bl)]
vpn <-  vpn[!(vpn %in% exclusion.responses)]
vpn <-  vpn[!(vpn %in% c())]
vpn.n = length(vpn)
vps=vpn

pupildat <- data.frame()

for (vpn in vps) {
  code <- vpn
  print(code)
  
  pupildat.vp <- read.csv2(paste(path,"Data/Pupil/",vpn,"_pupil.csv",sep=""))
  pupildat.vp$vp <- code
  pupildat.vp <- pupildat.vp[, c(502, 1:501)]
  pupildat <- bind_rows(pupildat, pupildat.vp)
}

pupil <- data.frame()
seqdatall <- data.frame()

for (vpn in vps) {
  code <- vpn
  print(code)
  
  # Comine with trial-info
  seqdat <- read.csv2(paste(path,"Data/Analyse/prot/",vpn,"_BL.csv",sep=""))
  seqdatall <- bind_rows(seqdatall,seqdat)
  pupil <- merge(seqdatall, pupildat, by = c("vp", "trial"))
}

pupil <- pupil %>%
  arrange(vp, trial)

pupil_long <- pupil %>% pivot_longer(
  cols = starts_with("pd"),
  names_to = "time",
  names_prefix = "pd",
  values_to = "mmChange"
)
pupil_long$time <- as.numeric(pupil_long$time)

Baselines <- pupil_long %>%
  group_by(vp, trial)%>%
  dplyr::filter(time <= 100)%>%
  dplyr::filter(time > 0)%>%
  summarise(
    Baseline = mean(mmChange, na.rm = TRUE)
  )



pupil.plot = pupil_long%>% group_by(operant, valence, time) %>% 
  summarise(mmChange.se = se(mmChange, na.rm=T), mmChange = mean(mmChange, na.rm=T))%>%
  mutate(condition = case_when(
    operant == 1 & valence == 1 ~ "Operant active",
    operant == 1 & valence == 0 ~ "Prevent active",
    operant == 0 & valence == 1 ~ "Operant passiv",
    operant == 0 & valence == 0 ~ "Prevent passive"
  ))%>%
  mutate(ActPass = case_when(
    operant == 1  ~ "active",
    operant == 0 ~ "passive"
  ))%>%
  mutate(OpPrev = case_when(
    valence == 1  ~ "operant",
    valence == 0 ~ "prevent"
  ))

print(pupil.plot %>% ggplot(aes(x=time, y=mmChange, color=condition, group=condition)) +
        #geom_dotplot(data=pupil.ga.gen.subj, mapping=aes(group=threat, fill=threat), binaxis="y", alpha=.25, color="black", stackratio=1, stackdir="centerwhole", dotsize=.5) +
        #geom_point() + geom_path(data=pupil.ga.gen %>% dplyr::filter(threat %in% c("CS-", "CS+")), color = "black", size=1.5) + #generalization line (geom_point first for order of x-axis)
        geom_line(linewidth=1) + #geom_point(size=4.5) +
        geom_ribbon(aes(ymin=mmChange-mmChange.se, ymax=mmChange+mmChange.se, color=condition), color = NA, alpha=.1) +
        scale_colour_viridis_d() +
        geom_line(aes(x = 100), color = "black") +
        geom_line(aes(x = 220), linetype = "dashed", color = "black") +
        annotate(geom = "text",
                 label = c("Cue Onset", "~ Picture Onset"),
                             x = c(110, 230),
                            y = c(3.4, 3.4),
                             angle = 90,
                  vjust = 1) +
        ylab("Pupil Size Change (mm)") + xlab("Time") + labs(color="condition") +
        theme_bw() + theme(
          #aspect.ratio = 1,
          legend.position = "right",
          panel.background = element_rect(fill="white", color="white"),
          legend.background = element_rect(fill="white", color="grey"),
          legend.key=element_rect(fill='white'),
          legend.text = element_text(size=14, color="black"),
          legend.title = element_text(size=14, color="black"),
          axis.text = element_text(color="black"),
          axis.text.x = element_text(size=16, color="black"),
          axis.text.y = element_text(size=16, color="black"),
          strip.text.x = element_text(size=12, color="black"),
          axis.ticks.x = element_line(color="black"),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          axis.title = element_text(size=16, color="black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0, "pt"))))


# Analyze just Anticipation Period ----------------------------------------------------------------------------------------------------
#pupil_long <- read.csv2(paste(path,"Data/Pupil/","Pupil_long.csv",sep=""))%>% select(-1)
pupildat <- data.frame()

for (vpn in vps) {
  code <- vpn
  print(code)
  
  pupildat.vp <- read.csv2(paste(path,"Data/Pupil/",vpn,"_pupil_anticipation.csv",sep=""))
  pupildat.vp$vp <- code
  pupildat.vp <- pupildat.vp[, c(162, 1:161)]
  pupildat <- bind_rows(pupildat, pupildat.vp)
}

pupil <- data.frame()
seqdatall <- data.frame()

for (vpn in vps) {
  code <- vpn
  print(code)
  
  # Comine with trial-info
  seqdat <- read.csv2(paste(path,"Data/Analyse/prot/",vpn,"_BL.csv",sep=""))
  seqdatall <- bind_rows(seqdatall,seqdat)
  pupil <- merge(seqdatall, pupildat, by = c("vp", "trial"))
}

pupil <- pupil %>%
  arrange(vp, trial)

pupil_long <- pupil %>% pivot_longer(
  cols = starts_with("pd"),
  names_to = "time",
  names_prefix = "pd",
  values_to = "mmChange"
)

pupil_long$time <- as.numeric(pupil_long$time)

pupil_long <- merge(pupil_long, Baselines, by = c("vp", "trial"))
pupil_long <- pupil_long %>%
  mutate(
    mmChange = mmChange - Baseline, 
    time = (time -160) *10
  )%>%
  mutate(OpPrev = case_when(valence == 1 ~ "operant",
                            valence == 0 ~ "prevent"),
         ActPass = case_when(operant == 1 ~ "active",
                             operant == 0 ~ "passive"))%>%
  mutate(condition = case_when(
    operant == 1 & valence == 1 ~ "Operant active",
    operant == 1 & valence == 0 ~ "Prevent active",
    operant == 0 & valence == 1 ~ "Operant passiv",
    operant == 0 & valence == 0 ~ "Prevent passive"
  ))


pupil.plot = pupil_long %>% group_by(condition, time) %>% 
  summarise(mmChange.se = se(mmChange, na.rm=T), mmChange = mean(mmChange, na.rm=T))


pupil.plot.1 <- pupil.plot %>% ggplot(aes(x=time, y=mmChange, group=condition)) + 
        #geom_dotplot(data=pupil.ga.gen.subj, mapping=aes(group=threat, fill=threat), binaxis="y", alpha=.25, color="black", stackratio=1, stackdir="centerwhole", dotsize=.5) +
        #geom_point() + geom_path(data=pupil.ga.gen %>% dplyr::filter(threat %in% c("CS-", "CS+")), color = "black", size=1.5) + #generalization line (geom_point first for order of x-axis)
        geom_line(aes(colour = condition, linetype = condition), linewidth = 1) +
        geom_ribbon(aes(ymin=mmChange-mmChange.se, ymax=mmChange+mmChange.se, fill= condition), color =NA, alpha=.1) + 
        scale_linetype_manual(name="Trial type", values=c("solid", "dotdash", "solid", "dotdash"), labels = c("active production","passiv production","active prevention", "passive prevention"))+
        scale_color_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("active production","passiv production","active prevention", "passive prevention")) +
        scale_fill_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("active production","passiv production","active prevention", "passive prevention")) +
        #scale_color_manual(values = c("#6BBFA3", "#007AC3")) +
        #scale_colour_viridis_d() +
        geom_line(aes(x = 0), color = "black") +
        #geom_line(aes(x = 220), linetype = "dashed", color = "black") +
        annotate(geom = "text",
                 label = c("Picture Onset"),
                 x = c(10),
                 y = c(0),
                 angle = 90,
                 vjust = 1) +
        ylab("Pupil Size Change (mm)") + xlab("Time") + labs(color="condition") +
        ggtitle("Experiment 1")+
        theme_classic()+
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18),
              legend.position = c(0.2, 0.8))

pupil.plot.2 <- pupil.plot %>% ggplot(aes(x=time, y=mmChange, group=condition)) + 
  #geom_dotplot(data=pupil.ga.gen.subj, mapping=aes(group=threat, fill=threat), binaxis="y", alpha=.25, color="black", stackratio=1, stackdir="centerwhole", dotsize=.5) +
  #geom_point() + geom_path(data=pupil.ga.gen %>% dplyr::filter(threat %in% c("CS-", "CS+")), color = "black", size=1.5) + #generalization line (geom_point first for order of x-axis)
  geom_line(aes(colour = condition, linetype = condition), linewidth = 1) +
  geom_ribbon(aes(ymin=mmChange-mmChange.se, ymax=mmChange+mmChange.se, fill= condition), color =NA, alpha=.1) + 
  scale_linetype_manual(name="Trial type", values=c("solid", "dotdash", "solid", "dotdash"), labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive"))+
  scale_color_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive")) +
  scale_fill_manual(name="Trial type", values = c("#708B50","#708B50","#E88067", "#E88067"),labels = c("Operant active","Operant passiv","Prevent active", "Prevent passive")) +
  #scale_color_manual(values = c("#6BBFA3", "#007AC3")) +
  #scale_colour_viridis_d() +
  geom_line(aes(x = 0), color = "black") +
  #geom_line(aes(x = 220), linetype = "dashed", color = "black") +
  annotate(geom = "text",
           label = c("Picture Onset"),
           x = c(10),
           y = c(0),
           angle = 90,
           vjust = 1) +
  ylab("Pupil Size Change (mm)") + xlab("Time") + labs(color="condition") +
  #ggtitle("Experiment 1")+
  ggtitle("Experiment 2")+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18),
        legend.position = "none")

cowplot::plot_grid(pupil.plot.1, pupil.plot.2, labels = c('A)', 'B)'), label_size = 14)

write.csv2(pupil_long,paste(path,"Data/Pupil/","Pupil_long.csv",sep=""))

# Analysen 

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

