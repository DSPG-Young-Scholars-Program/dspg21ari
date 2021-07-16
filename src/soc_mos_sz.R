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

#recast data table to which skills correspond to which MOS

for (i in 1:nrow(mos_skills)){
  rank_skills<-unlist(mos_skills[i,2])
  a<-0
  b<-0
  temp<-list()
  for(j in 1:nrow(skills_top)){
    for(k in 1:length(rank_skills)){
      if(skills_top[j, 1]==rank_skills[k]){
        a<-a+1
        temp[a]<-rank_skills[k]
      }
    }
  }
  mos_skills[i, 3]<-list(temp)
}
write_rds(mos_skills, "./data/working/mos_soc.Rds")

#quick personal sanity check
check<-matrix(nrow=11, ncol=11)
for(x in 1:11){
  for(y in 1:11){
    check[x, y]<-identical(mos_skills[x, 3], mos_skills[y, 3])
  }
}
