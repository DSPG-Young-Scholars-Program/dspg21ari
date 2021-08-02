### 0: packages
library(data.table)
library(tidytable)
library(maditr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stargazer)

### 1: files
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


#pull top 40 jobs by employment + annual median income - commmented
#soc<-soc[order(-tot_emp, -a_median)]
# soc_top_20<-soc[1:20, ]$onetname


### 2:
#order soc skills by employment + median income - morgan removed selecting top 30,000 skills
soc_skills<-soc_skills[order(-tot_emp, -a_median)]
skills_top<-sort(table(soc_skills$skill), decreasing=TRUE)
skills_top<-skills_top%>% sort(decreasing = TRUE) %>% as.data.table()


#2.1: match by skills w/highest frequency
dt_skill_temp<-data.table(mos=mos_skills[1:11,1], skill=character())
for (i in 1:nrow(mos_skills)){
  rank_skills<-unlist(mos_skills[i,2])
  a<-0
  temp<-list()
  for(j in 1:nrow(skills_top)){
    for(k in 1:length(rank_skills)){
      if(skills_top[j, 1]==rank_skills[k]){
        a<-a+1
        temp[a]<-rank_skills[k]
      }
    }
  }
  dt_skill_temp[i, 2]<-list(temp)
}
setnames(dt_skill_temp, "mos.Army MOS Code", "Army MOS Code")
mos_skills<-merge( mos_skills, dt_skill_temp,by="Army MOS Code")
#mos_skills<-as.data.frame(mos_skills)
write_rds(mos_skills, "./data/working/mos_soc.Rds")
write.csv(mos_skills, "./data/working/mos_soc.csv", col.names = T, row.names = F)

#quick personal sanity check
sanity_check<-matrix(nrow=10, ncol=10)
for(x in 1:10){
  for(y in 1:10){
    sanity_check[x, y]<-identical(mos_skills[x, 3], mos_skills[y, 3])
  }
}

#thought: each mos has different employment estimates--weight individually? make separate tables?
#2.2: match by overall employment weights
colnames(skills_top)<-c("target", "N")
weighted_skill_weights<-weighted_skill %>% select("target", "e_weight") %>% distinct()
skills_top<-left_join(skills_top, weighted_skill_weights)
skills_top<-skills_top[order(-e_weight)] #weight by employment weights
mos_skills_weights<-mos_skills[,1:2]

dt_skill_temp<-data.table(mos=mos_skills[1:11,1], skill=character())
for (i in 1:nrow(mos_skills)){
  rank_skills<-unlist(mos_skills[i,2])
  a<-0
  temp<-list()
  for(j in 1:nrow(skills_top)){
    for(k in 1:length(rank_skills)){
      if(skills_top[j, 1]==rank_skills[k]){
        a<-a+1
        temp[a]<-rank_skills[k]
      }
    }
  }
  dt_skill_temp[i, 2]<-list(temp)
}
setnames(dt_skill_temp, "mos.Army MOS Code", "Army MOS Code")
mos_skills_ov_weight<-merge(mos_skills_weights, dt_skill_temp,by="Army MOS Code")

#2.3: weight skills within each mos then match
mos<-unique(weighted_skill$source)
mos_skill_weighted<-data.table(mos=mos, skills=character())
mos_tab_list<-list()
for(i in 1:length(mos)){
  temp<-weighted_skill %>% filter(source==mos[i]) %>% select("source", "target", "employ", "salary", "freq", "e_total", "s_total")
  #temp<-temp %>% group_by(target) %>% mutate(freq_mos=n()) %>% ungroup()

  temp<-temp %>% mutate(mos_e_total=sum(employ))%>% mutate(mos_e_weight=employ/mos_e_total)
  temp<-temp %>% mutate(mos_s_total=sum(salary))%>% mutate(mos_s_weight=salary/mos_s_total)
  temp<-temp[order(-freq, -mos_e_weight)]
  mos_skill_weighted[i, 2]<-list(temp$target)

  assign(paste(mos[i], "skills", sep='-'), temp)
  mos_tab_list[i]<-paste(mos[i], "skills", sep='-')
}
mos_tab_list<-unlist(mos_tab_list)

write_rds(mos_skill_weighted, "./data/working/mos_soc_weighted.Rds")

### 3: all skills
all_skills_mos_specialized<-all_skills_mos %>% filter.(isspecialized==TRUE)
all_skills_mos_software<-all_skills_mos %>% filter.(issoftware==TRUE)
baseline_skills<-skills_top

#reshape(all_skills_mos, "Army MOS Title", "skill", direction="wide")

all_skills_mos_specialized_top<-all_skills_mos_specialized %>% group_by(`Army MOS Title`, skill) %>%
  summarise(freq=n()) %>% select(`Army MOS Title`, skill, freq) %>% as.data.table() %>% setkey(freq)
all_skills_mos_specialized_top<-all_skills_mos_specialized_top[, tail(.SD, 10), by=`Army MOS Title`]


all_skills_mos_software_top<-all_skills_mos_software %>% group_by(`Army MOS Title`, skill) %>%
  summarise(freq=n()) %>% select(`Army MOS Title`, skill, freq) %>% as.data.table() %>% setkey(freq)
all_skills_mos_software_top<-all_skills_mos_software_top[, tail(.SD, 10), by=`Army MOS Title`]
### 4: EDA, viz(updated: 05_eda_viz)

# 3.1: soc_skills [sector frequency, skills, cross tabs?]
sect_skills<-soc_skills %>% select(sectorname, skill)
freq_skill<-table(sect_skills$skill) %>% as.data.table()
freq_skill<-freq_skill[order(-N)] %>% top_n.(8)

# frequency of sector, basic skills:
ggplot(freq_skill, aes(x=reorder(V1, -N), y=N))+geom_bar(stat='identity')

#all mos skills
freq_all_skill_mos<-table(all_skills_mos$skill) %>% as.data.table()
freq_all_skill_mos<-freq_all_skill_mos[order(-N)]%>% top_n.(12)
ggplot(freq_all_skill_mos, aes(x=reorder(V1, -N), y=N))+geom_bar(stat='identity')

#all soc code skills
freq_all_skills_soc<-table(all_skills_soc$skill) %>% as.data.table()
freq_all_skills_soc<-freq_all_skills_soc[order(-N)] %>% top_n.(12)

ggplot(freq_all_skills_soc, aes(x=reorder(V1, -N), y=N))+geom_bar(stat='identity')

#bls/wage information
all_skills_wage<-all_skills_soc %>% select(socname, tot_emp, a_mean) %>% unique()
all_skills_wage<-all_skills_wage[complete.cases(all_skills_wage), ]
all_skills_wage[,3]<-sapply(all_skills_wage[,3], as.numeric)

ggplot(all_skills_wage, aes(tot_emp))+geom_histogram()
ggplot(all_skills_wage, aes(a_mean))+geom_histogram()
summary(all_skills_wage)

ggplot(all_skills_wage, aes(tot_emp))+geom_boxplot()
ggplot(all_skills_wage, aes(a_mean))+geom_boxplot()



#colors
colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

weighted_skill %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
  group_by(target) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(target, freq))) +
  scale_fill_manual(values=colors[1:10]) +
  geom_col(show.legend = FALSE) +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

# unique skills
all_skill_uq %>%
  na.omit()%>%
  select(target, source, e_freq) %>%
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
=======
### 3: EDA, viz

# 3.1: soc_skills [sector frequency, skills, cross tabs?]
sect_skills<-soc_skills %>% select(sectorname, skill)
freq_sector<-table(sect_skills$sectorname) %>% as.data.table()

# frequency of skills:
ggplot(freq_sector, aes(x=V1, y=N))+geom_bar(stat='identity')
>>>>>>> 277cb8bfbfcb512b3e653da1abc06902fc70620c
