rm(list=ls()) 
options(digits = 7)
library(tidyverse)
library(data.table)

dat <- data.table::fread("dat_rating.csv", sep = ",", dec = ".")

dat <- dat %>% ungroup() %>% group_by(vp, posneg) %>% summarize(
  mean_valence = mean(valence),
  mean_arousal = mean(arousal)
)

dat_pos <- dat %>% ungroup() %>% filter(posneg == "positive")
dat_neg <- dat %>% ungroup() %>% filter(posneg == "negative")

t.test(dat_pos$mean_valence, dat_neg$mean_valence, alternative = "greater", paired = T, var.equal = T) %>% schoRsch::t_out()
t.test(dat_pos$mean_arousal, dat_neg$mean_arousal, alternative = "two.sided", paired = T, var.equal = T) %>% schoRsch::t_out()
dat %>% ungroup() %>% group_by(posneg) %>% summarize(
  meanmean_valence = mean(mean_valence),
  meanmean_arousal = mean(mean_arousal)
)

dat_valence_d <- dat %>% select(-mean_arousal) %>%  
  pivot_wider(names_from = c("posneg"), values_from = "mean_valence") %>%
  mutate(
    d_val = positive - negative
  ) 

dat_valence_join <- dat %>% full_join(dat_valence_d) %>%
  group_by(posneg) %>% summarize(
    m_val = mean(mean_valence),
    se_val = sd(d_val) /sqrt(n())
  )

ggplot(data = dat_valence_join, mapping = aes(x = posneg, y = m_val, group = 1)) +
  geom_point(size = 2, color = "#6BBFA3") +
  geom_line(linewidth = 1, color = "#6BBFA3") +
  geom_errorbar(aes(ymin = m_val - se_val, ymax = m_val + se_val), width = 0.05, color = "#6BBFA3") +
  xlab("Action Type") +
  ylab("Mean Valence Ratings +- SE_PD") +
  theme_classic()
