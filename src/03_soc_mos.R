### 0: packages
library(data.table)
library(tidytable)
library(maditr)
library(tidyverse)
library(readxl)
library(ggplot2)

### 1: files
mos_skills<-readRDS("data/working/mos_skill.Rds") %>% as.data.table()
soc_skills<-readRDS("data/working/soc_skill_bls_long.Rds")%>% as.data.table()
weighted_skill<-weighted_skill<-read_csv("data/working/mos_skill_network.csv") %>% as.data.table()
#soc<-readRDS("data/working/soc_skill_bls.Rds")

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

### 3: EDA, viz

# 3.1: soc_skills [sector frequency, skills, cross tabs?]
sect_skills<-soc_skills %>% select(sectorname, skill)
freq_sector<-table(sect_skills$sectorname) %>% as.data.table()

# frequency of skills:
ggplot(freq_sector, aes(x=V1, y=N))+geom_bar(stat='identity')
