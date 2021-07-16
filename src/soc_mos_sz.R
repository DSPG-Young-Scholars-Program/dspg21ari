### 0: packages
library(data.table)
library(tidytable)
library(maditr)
library(tidyverse)
library(readxl)

### 1: files
mos_skills<-readRDS("data/working/mos_skill.Rds") %>% as.data.table()
soc_skills<-readRDS("data/working/soc_skill_bls_long.Rds")%>% as.data.table()
soc<-readRDS("data/working/soc_skill_bls.Rds")

#pull top 40 jobs by employment + annual median income
soc<-soc[order(-tot_emp, -a_median)]
soc_top_20<-soc[1:20, ]$onetname

### 2:
#order soc skills by employment + median income
soc_skills<-soc_skills[order(-tot_emp, -a_median)]
soc_skills_top<-soc_skills[1:30000, ]
skills_top<-as.data.table(table(soc_skills_top$skill))
skills_top<-skills_top%>% arrange.(skills_top, desc(N))

#match by most common skills
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

#quick personal sanity check
sanity_check<-matrix(nrow=10, ncol=10)
for(x in 1:10){
  for(y in 1:10){
    sanity_check[x, y]<-identical(mos_skills[x, 3], mos_skills[y, 3])
  }
}
