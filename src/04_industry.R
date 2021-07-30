library(tidyverse)
library(data.table)
library(readxl)

`%notin%`= function(x,y) !(x %in% y)
colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

#
# Industry Case Study ------------------------------------
#

# get_db_conn <-
#   function(db_name = "sdad",
#            db_host = "postgis1",
#            db_port = "5432",
#            db_user = Sys.getenv("db_usr"),
#            db_pass = Sys.getenv("db_pwd")) {
#     RPostgreSQL::dbConnect(
#       drv = RPostgreSQL::PostgreSQL(),
#       dbname = db_name,
#       host = db_host,
#       port = db_port,
#       user = db_user,
#       password = db_pass
#     )
#   }
#
# con <- get_db_conn()
#
#
# health_job <- DBI::dbGetQuery(con, "SELECT id, jobdate, soc, socname, occfam, occfamname, onet, onetname, sectorname FROM bgt_job.main WHERE jobdate >= '2019-01-01' AND jobdate < '2020-01-01'
#                                 AND sectorname in ('Health Care and Social Assistance')"
# )
#
# # Skills from 2019 --------------------------------------
#
# all_skill <- DBI::dbGetQuery(con, "SELECT id, jobdate, skill, isspecialized, issoftware, isbaseline FROM bgt_job.skill WHERE jobdate >= '2019-01-01' AND jobdate < '2020-01-01'")
#
# DBI::dbDisconnect(con)
#
# # Health Job Skills DF ----------------------------------
# health_soc <- data.table(health_job, key="id")[
#   data.table(all_skill, key="id"),
#   allow.cartesian=TRUE]
# rm(health_job)
# rm(all_skill)
# health_soc <- health_soc[is.na(onet)==F,]
#
# crosswalk <- read_excel("./data/original/crosswalk.xlsx")
# soc0 <- unique(as.vector(crosswalk$`O*NET-SOC Code`))
# army_health <- filter(health_soc, onet %in% soc0)
# non_army_health <- filter(health_soc, onet %notin% soc0)
#
# army_health <- select(army_health, soc, socname, onet, onetname, skill, isspecialized, issoftware, isbaseline)
# army_health <- unique(army_health)
#
# non_army_health <- select(non_army_health, soc, socname, onet, onetname, skill, isspecialized, issoftware, isbaseline)
# non_army_health <- unique(non_army_health)
#
# bls <- read_excel("./data/original/bls.xlsx")
# army_health <- left_join(army_health, bls, by = c("soc" = "occ_code"))
# army_health <- select(army_health, soc, onet, socname, onetname, skill, tot_emp, emp_prse, h_mean, a_mean, isspecialized, issoftware, isbaseline)
# army_health <- left_join(crosswalk, army_health, by = c("O*NET-SOC Code" = "onet"))
# army_health <- unique(army_health)
#
# non_army_health <- left_join(non_army_health, bls, by = c("soc" = "occ_code"))
# non_army_health <- select(non_army_health, soc, onet, socname, onetname, skill, tot_emp, emp_prse, h_mean, a_mean, isspecialized, issoftware, isbaseline)
# non_army_health <- unique(non_army_health)

# write.csv(army_health, "./data/working/army_health_skills.csv")
# write.csv(non_army_health, "./data/working/non_army_health_skills.csv")

army_health <- read.csv("./data/working/army_health_skills.csv")
non_army_health <- read.csv( "./data/working/non_army_health_skills.csv")

# 7741 skills that intersect out of 11,958 possible = 64.73 percent coverage
both_skills <- intersect(non_army_health$skill, army_health$skill)

non_army_health$skill_type <- ifelse(non_army_health$isbaseline == T, "Baseline",
                                    ifelse(non_army_health$issoftware == T, "Software", "Specialized"))

army_health$skill_type <- ifelse(army_health$isbaseline == T, "Baseline",
                                     ifelse(army_health$issoftware == T, "Software", "Specialized"))

non_army_health <- non_army_health %>%
  group_by(skill) %>%
  mutate(freq = n()) %>%
  ungroup() %>%
  mutate(e_total=sum(tot_emp,na.rm = T)) %>%
  group_by(skill) %>%
  mutate(e_weight=sum(tot_emp/e_total,na.rm = T)) %>%
  ungroup() %>%
  mutate(e_freq = e_weight*freq)

army_health <- army_health %>%
  group_by(skill) %>%
  mutate(freq = n()) %>%
  ungroup() %>%
  mutate(e_total=sum(tot_emp,na.rm = T)) %>%
  group_by(skill) %>%
  mutate(e_weight=sum(tot_emp/e_total,na.rm = T)) %>%
  ungroup() %>%
  mutate(e_freq = e_weight*freq)

non_army_health$skill_type <- as.factor(non_army_health$skill_type)
army_health$skill_type <- as.factor(army_health$skill_type)

non_army_health %>%
  select(skill, skill_type, e_freq) %>%
  group_by(skill) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(skill_type) %>%
  unique()%>%
  slice_max(freq, n = 10) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(skill, freq), fill = skill_type)) +
  scale_fill_manual(values=c(colors[1], colors[4], colors[9])) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~skill_type, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Non-Army SOC Code Skills")

army_health %>%
  na.omit()%>%
  select(skill, skill_type, e_freq) %>%
  group_by(skill) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(skill_type) %>%
  unique()%>%
  slice_max(freq, n = 10) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(skill, freq), fill = skill_type)) +
  scale_fill_manual(values=c(colors[1], colors[4], colors[9])) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~skill_type, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")

non_army_health %>%
  select(skill, skill_type, e_freq) %>%
  filter(skill %notin% both_skills) %>%
  group_by(skill) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(skill_type) %>%
  unique()%>%
  slice_max(freq, n = 10) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(skill, freq), fill = skill_type)) +
  scale_fill_manual(values=c(colors[1], colors[4], colors[9])) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~skill_type, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Non-Army SOC Code Skills")

army_health %>%
  na.omit()%>%
  select(skill, skill_type, e_freq) %>%
  filter(skill %notin% both_skills) %>%
  group_by(skill) %>%
  mutate(freq = sum(e_freq)) %>%
  ungroup() %>%
  group_by(skill_type) %>%
  unique()%>%
  slice_max(freq, n = 10) %>%
  ungroup() %>%
  ggplot(aes(freq, fct_reorder(skill, freq), fill = skill_type)) +
  scale_fill_manual(values=c(colors[1], colors[4], colors[9])) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~skill_type, ncol = 1, scales = "free") +
  labs(x = "Skill Frequency Weighted by Employment", y = NULL,
       title = "Army SOC Code Skills")
