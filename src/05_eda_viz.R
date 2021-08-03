### TO DO
# -stacked barcharts (clean df for that)
# -make df

### 0: packages
library(data.table)
library(tidytable)
library(maditr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stargazer)

### 1: load in files

# baseline
mos_skills<-readRDS("data/working/mos_skill.Rds") %>% as.data.table()
soc_skills<-readRDS("data/working/soc_skill_bls_long.Rds")%>% as.data.table()
skill_uq<-read_csv("data/working/skill_network_unique.csv") %>% as.data.table()
#soc<-readRDS("data/working/soc_skill_bls.Rds")

#all skills df
all_skills_mos<-readRDS("data/working/all_mos_skill_long.Rds") %>% as.data.table()
all_skills_soc<-readRDS("data/working/all_soc_skill_bls_long.Rds") %>% as.data.table()
all_skill_uq<-read_csv("data/working/all_skill_unique.csv")
all_skill<-read_csv("data/working/all_skill_mos_network.csv")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

mos_1<-c("Cannon Crewmember", "Cavalry Scout", "Combat Engineer", "Combat Medic Specialist", "Infantryman")
mos_2<-c('Intelligence Analyst', "M1 Armor Crewman", "Military Police", "Unit Supply Specialist", "Wheeled Vehicle Repairer")

### 2:baseline

## 2.1: EDA: summary statistics of wage/employment
baseline_skills_wage<-all_skills_soc %>% select(socname, tot_emp, a_mean) %>% unique()
baseline_skills_wage<-all_skills_wage[complete.cases(all_skills_wage), ]
baseline_skills_wage[,3]<-sapply(baseline_skills_wage[,3], as.numeric)
stargazer(baseline_skills_wage, summary = TRUE)

## 2.2: all baseline skill frequency (top 8)
freq_skill<-table(soc_skills$skill) %>% as.data.table()
freq_skill<-freq_skill[order(-N)] %>% top_n.(8)
ggplot(freq_skill, aes(x=reorder(V1, -N), y=N))+geom_bar(stat='identity', fill=colors[1]) +
  labs(x = "Baseline Skill Frequency", y = NULL, title = "Army SOC Code Baseline Skills")+
  scale_x_discrete(labels = str_wrap(freq_skill$V1, width = 10))

## 2.3: stacked bar chart (each bar is an MOS, each section is frequency of a top 10 skill)
all_skills_baseline<-all_skills_mos %>% filter.(isbaseline==TRUE)
all_skills_baseline_top<-all_skills_baseline %>% group_by(`Army MOS Title`, skill) %>%
  summarise(freq=n()) %>% select(`Army MOS Title`, skill, freq) %>% as.data.table() %>% setkey(freq)
all_skills_baseline_top<-all_skills_baseline_top[, tail(.SD, 10), by=`Army MOS Title`]

all_skills_baseline_top<-all_skills_baseline_top %>%
  mutate.(percentage=freq/sum(freq), .by=`Army MOS Title`) %>% ungroup()
ggplot(all_skills_baseline_top, aes(fill=skill, y=freq, x=`Army MOS Title`))+
  geom_bar(position='fill', stat='identity')+
  labs(x="Army MOS", y="Percentage", title="Percentage of Skill in the Top 10 Baseline skills for a MOS")+
  scale_x_discrete(labels=str_wrap(unique(all_skills_baseline_top$`Army MOS Title`), width=10))+
  theme(text=element_text(size=9))

## 2.4: unique baseline skills + frequency/MOS (maybe top 3 per MOS?)
skill_uq %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_1) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

skill_uq %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_2) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")


### 3: all skills

## 3.1: EDA: Summary statistics, wage, employment
all_skills_wage<-all_skills_soc %>% select(socname, tot_emp, a_mean) %>% unique()
all_skills_wage<-all_skills_wage[complete.cases(all_skills_wage), ]
all_skills_wage[,3]<-sapply(all_skills_wage[,3], as.numeric)
stargazer(all_skills_wage, summary = TRUE)

## 3.2: all skills frequency
freq_all_skill<-table(all_skills_mos$skill) %>% as.data.table()
freq_all_skill<-freq_all_skill[order(-N)] %>% top_n.(10)
ggplot(freq_all_skill, aes(x=reorder(V1, -N), y=N))+geom_bar(stat='identity', fill=colors[1]) +
  labs(x = "Skill Frequency", y = NULL, title = "Army SOC Code Skills (All)")+
  scale_x_discrete(labels = str_wrap(freq_all_skill$V1, width = 8))


## 3.3: software + specialized skills frequency
# specialized
all_skills_mos_specialized<-all_skill %>% filter.(skill_type=="Specialized")
freq_specialized_skill<-all_skills_mos_specialized %>% select(target, freq) %>% as.data.table()
freq_specialized_skill<-freq_specialized_skill[order(-freq)] %>% unique() %>% top_n.(10)

ggplot(freq_specialized_skill, aes(x=reorder(target, -freq), y=freq))+geom_bar(stat='identity', fill=colors[1]) +
  labs(x = "Skill Frequency", y = NULL, title = "Army SOC Code Skills (Specialized)")+
  scale_x_discrete(labels = str_wrap(freq_specialized_skill$target, width = 8))

# software

all_skills_mos_software<-all_skill %>% filter.(skill_type=="Software")
freq_software_skill<-all_skills_mos_software %>% select(target, freq) %>% as.data.table()
freq_software_skill<-freq_software_skill[order(-freq)] %>% unique() %>% top_n.(10)

ggplot(freq_software_skill, aes(x=reorder(target, -freq), y=freq))+geom_bar(stat='identity', fill=colors[1]) +
  labs(x = "Skill Frequency", y = NULL, title = "Army SOC Code Skills (Softwawre)")+
  scale_x_discrete(labels = str_wrap(freq_software_skill$target, width = 8))

## 3.4: software, specialized skills by MOS

#specialized/MOS
all_skills_mos_specialized %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_2) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

all_skills_mos_specialized %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_1) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

#software/mos
all_skills_mos_software %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_1) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

all_skills_mos_software %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_2) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")
## 3.4: stacked barchart?
# Specialized
all_skills_specialized_mos<-all_skills_mos %>% filter(isspecialized==TRUE)
all_skills_mos_specialized_top<-all_skills_specialized_mos %>% group_by(`Army MOS Title`, skill) %>%
  summarise(freq=n()) %>% select(`Army MOS Title`, skill, freq)
all_skills_mos_specialized_top<-all_skills_mos_specialized_top%>% as.data.table() %>% setkey(freq)
all_skills_mos_specialized_top<-all_skills_mos_specialized_top[, tail(.SD, 10), by=`Army MOS Title`]

all_skills_mos_specialized_top<-all_skills_mos_specialized_top %>%
  mutate.(percentage=freq/sum(freq), .by=`Army MOS Title`) %>% ungroup()
ggplot(all_skills_mos_specialized_top, aes(fill=str_wrap(skill, 10), y=freq, x=`Army MOS Title`))+
  geom_bar(position='fill', stat='identity')+
  labs(x="Army MOS", y="Percentage", title="Percentage of Skill in the Top 10 Baseline skills for a MOS", fill="Skill")+
  scale_x_discrete(labels=str_wrap(unique(all_skills_mos_specialized_top$`Army MOS Title`), width=10))+
  theme(text=element_text(size=9), legend.text=element_text(size=7))


# Software
all_skills_software_mos<-all_skills_mos %>% filter(issoftware==TRUE)
all_skills_mos_software_top<-all_skills_software_mos %>% group_by(`Army MOS Title`, skill) %>%
  summarise(freq=n()) %>% select(`Army MOS Title`, skill, freq) %>% as.data.table() %>% setkey(freq)
all_skills_mos_software_top<-all_skills_mos_software_top[, tail(.SD, 10), by=`Army MOS Title`]

all_skills_mos_software_top<-all_skills_mos_software_top %>%
  mutate.(percentage=freq/sum(freq), .by=`Army MOS Title`) %>% ungroup()
ggplot(all_skills_mos_software_top, aes(fill=str_wrap(skill, 10), y=freq, x=`Army MOS Title`))+
  geom_bar(position='fill', stat='identity')+
  labs(x="Army MOS", y="Percentage", title="Percentage of Skill in the Top 10 Baseline skills for a MOS", fill="Skill")+
  scale_x_discrete(labels=str_wrap(unique(all_skills_mos_software_top$`Army MOS Title`), width=10))+
  theme(text=element_text(size=9), legend.text=element_text(size=7))

## 3.5: unique skills
all_skill_uq %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_2) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

all_skill_uq %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  filter(source %in% mos_1) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(source) %>%
  unique()%>%
  slice_max(freq, n = 5) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq), fill = source)) +
  scale_fill_manual(values=colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

## 3.6: salary/employment by MOS

# employment, group 1
all_skill %>%
  na.omit()%>%
  select(target, source, employ) %>%
  filter(source %in% mos_1) %>%
  ggplot(aes(source, employ)) +
  stat_summary(fun=mean, geom="bar", fill=colors[1])+
  labs(x = "MOS", y = NULL,
       title = "Average Employment per MOS (1)")

# employment, group 2
all_skill %>%
  na.omit()%>%
  select(target, source, employ) %>%
  filter(source %in% mos_2) %>%
  ggplot(aes(source, employ)) +
  stat_summary(fun=mean, geom="bar", fill=colors[1])+
  labs(x = "MOS", y = NULL,
       title = "Average Employment per MOS (2)")

# salary, group 1
all_skill %>%
  na.omit()%>%
  select(target, source, salary) %>%
  filter(source %in% mos_1) %>%
  ggplot(aes(source, salary)) +
  stat_summary(fun=mean, geom="bar", fill=colors[1])+
  labs(x = "MOS", y = NULL,
       title = "Average Income per MOS (1)")


# salary, group 2
all_skill %>%
  na.omit()%>%
  select(target, source, salary) %>%
  filter(source %in% mos_2) %>%
  ggplot(aes(source, salary)) +
  stat_summary(fun=mean, geom="bar", fill=colors[1])+
  labs(x = "MOS", y = NULL,
       title = "Average Income per MOS (2)")
