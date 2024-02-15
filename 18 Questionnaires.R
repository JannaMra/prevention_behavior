############################################################################
#Prevention
#

# read in questionnaires
questionnaires = suppressMessages( #avoid name repair message (affected columns will be deselected anyway)
  readxl::read_excel("Fragebogen_Daten.xlsx" %>% paste0(path, "Data/", .),
                     sheet = "Fragebögen_Gesamt", col_names=T)) %>% rename(subject = `vp_id`) %>%
  filter(subject %>% is.na() == F, subject > 0, #get rid of test subject & empty rows
         subject %in% exclusion.responses == F) #apply a priori exclusions

# M and SD questionnaires
questionnaires %>% summarize(mean = mean(UI), SD = sd(UI))
questionnaires %>% summarize(mean = mean(BAS), SD = sd(BAS))
questionnaires %>% summarize(mean = mean(ASI), SD = sd(ASI))

# Cut-Offs?
# paste0("subjects meeting the cut-off: ", {mean(questionnaires$SPAI >= spai.cutoff)*100} %>% round(digits=2), "% ", 
#        "(N = ", sum(questionnaires$SPAI > spai.cutoff), "; ",
#        "z = ", with(questionnaires, (spai.cutoff - mean(SPAI)) / sd(SPAI)) %>% round(digits=2), ")")

# plot distribution 
print(ui.plot <- questionnaires %>%
        ggplot(aes(x=UI)) + geom_histogram(binwidth=1, color="black", fill="grey") + 
        myGgTheme + scale_y_continuous(breaks=scales::breaks_pretty()))

print(asi.plot <- questionnaires %>%
        ggplot(aes(x=ASI)) + geom_histogram(binwidth=1, color="black", fill="grey") + 
        myGgTheme + scale_y_continuous(breaks=scales::breaks_pretty()))

# correlations questionnaires
questionnaires %>% with(cor.test(ASI, UI, alternative="greater")) %>% correlation_out()
questionnaires %>% with(cor.test(BAS, ASI, alternative="greater")) %>% correlation_out()
questionnaires %>% with(cor.test(UI, BAS, alternative="greater")) %>% correlation_out()
