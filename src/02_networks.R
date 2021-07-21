library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)

install.packages(c("igraph","graphlayouts","ggraph","ggplot2"))
#
# Network Visualizations -----------------------------
#

# http://mr.schochastics.net/netVizR.html - walkthrough on network visualizations
# Visualizations
# MOS - Skill - Salary or Employment (size)
# MOS - Onetname - Employment (size)
# Onetname - Skill - Employment (size) - MOS (color)

# Data Frames to Export + Use
socs <- read_rds("./data/working/soc_skill_bls_long.Rds")
crosswalk <- read_excel("./data/original/crosswalk.xlsx")

# MOS to SOC (Long)
mos_skill_long <- left_join(crosswalk, socs, by = c("O*NET-SOC Code" = "onet"))
soc_mos <- mos_skill_long %>% transmute(soure = `Army MOS Title`, target = socname, employ = tot_emp, salary = as.numeric(a_mean)) %>% unique()
soc_mos <- na.omit(soc_mos)
soc_mos <- soc_mos %>% group_by(target) %>% mutate(freq = n()) %>% ungroup()
soc_mos <- soc_mos %>%
  mutate(e_total=sum(employ)) %>%
  group_by(target) %>%
  mutate(e_weight=sum(employ/e_total)) %>%
  ungroup() %>%
  mutate(e_freq = e_weight*freq)
soc_mos <- soc_mos %>%
  mutate(s_total=sum(salary)) %>%
  group_by(target) %>%
  mutate(s_weight=sum(salary/s_total)) %>%
  ungroup() %>%
  mutate(s_freq = s_weight*freq)
write.csv(soc_mos, "./data/working/mos_soc_network.csv", row.names = F)

# MOS to Skill (Long)
skill_mos <- mos_skill_long %>% transmute(source = `Army MOS Title`, target = skill, employ = tot_emp, salary = as.numeric(a_mean)) %>% unique()
skill_mos <- na.omit(skill_mos)
skill_mos <- skill_mos %>% group_by(target) %>% mutate(freq = n()) %>% ungroup()
skill_mos <- skill_mos %>%
  mutate(e_total=sum(employ)) %>%
  group_by(target) %>%
  mutate(e_weight=sum(employ/e_total)) %>%
  ungroup() %>%
  mutate(e_freq = e_weight*freq)
skill_mos <- skill_mos %>%
  mutate(s_total=sum(salary, na.omit = T)) %>%
  group_by(target) %>%
  mutate(s_weight=sum(salary/s_total)) %>%
  ungroup() %>%
  mutate(s_freq = s_weight*freq)
write.csv(skill_mos, "./data/working/mos_skill_network.csv", row.names = F)

# MOS to MOS (Adjacency Matrix)

