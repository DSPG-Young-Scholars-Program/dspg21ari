library(tidyverse)
library(readxl)
library(data.table)

#
# SOC - Job Ads Skills -------------------------------------
#

# Unique MOS to SOCs -------------------------------------------
crosswalk <- read_excel("./data/original/crosswalk.xlsx")
soc0 <- unique(as.vector(crosswalk$`O*NET-SOC Code`)) #248 onet codes
# soc0 <- paste0(soc0, collapse = "','") #then add a beginning and trailing ' to create a pasted vector to filter

# Database Pull ------------------------------------------------
get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

con <- get_db_conn()


# job <- DBI::dbGetQuery(con, "SELECT id, jobdate, onet, onetname, sectorname FROM bgt_job.main WHERE jobdate >= '2019-01-01' AND jobdate < '2020-01-01'
#                        AND onet IN ('33-1021.00','33-1021.01','33-1021.02','33-2011.00','33-2011.01','33-2011.02','33-2021.00','33-2021.01',
# '33-2021.02','33-3051.00','33-3051.01','33-3051.03','47-2221.00','47-4071.00','49-1011.00','49-9044.00','49-9095.00','53-1011.00','53-5021.00',
# '53-5021.01','53-5021.02','53-5021.03','53-5031.00','11-9051.00','19-4093.00','29-2041.00','33-1011.00','33-1012.00','33-3012.00','33-3021.00',
# '33-3021.01','33-3021.02','33-3021.03','33-3021.05','33-3021.06','37-1012.00','45-1011.00','45-1011.05','45-1011.06','45-1011.07','45-1011.08',
# '47-1011.00','47-1011.03','47-2011.00','47-2021.00','47-2031.00','47-2031.01','47-2031.02','47-2081.00','47-2111.00','47-2152.00','47-2152.01',
# '47-2152.02','47-2211.00','47-3016.00','47-4021.00','47-4041.00','47-4051.00','47-4099.00','47-4099.02','47-4099.03','47-5011.00','47-5012.00',
# '47-5013.00','47-5031.00','47-5041.00','49-2022.00','49-2095.00','49-2096.00','49-2097.00','49-3011.00','49-3023.00','49-3023.01','49-3023.02',
# '49-3031.00','49-3042.00','49-9021.00','49-9021.01','49-9021.02','49-9051.00','49-9081.00','49-9091.00','49-9092.00','49-9099.00','49-9099.01',
# '51-1011.00','51-4011.00','51-4021.00','51-4111.00','51-8031.00','51-8091.00','51-8092.00','51-8093.00','51-8099.00','51-8099.01','51-8099.02',
# '51-8099.03','51-8099.04','51-9011.00','51-9021.00','53-1021.00','53-1021.01','53-1031.00','53-2012.00','53-4013.00','53-5022.00','53-6051.00',
# '53-6051.01','53-6051.07','53-6051.08','47-2171.00','47-2231.00','47-4061.00','47-4091.00','47-5081.00','51-9012.00','45-3011.00','47-2041.00',
# '53-4021.00','53-4031.00','53-4041.00','11-9071.00','29-2061.00','33-9011.00','11-9131.00','13-1022.00','19-4011.00','19-4011.01','19-4011.02',
# '25-1194.00','27-4012.00','27-4013.00','27-4014.00','31-9093.00','31-9099.00','31-9099.01','31-9099.02','33-9031.00','39-7012.00','41-1011.00',
# '41-9022.00','43-1011.00','43-5031.00','53-2021.00','47-2051.00','47-2071.00','47-2072.00','47-3012.00','49-2092.00','49-2093.00','49-3021.00',
# '49-3041.00','49-3043.00','49-3051.00','49-3052.00','49-3053.00','49-9041.00','49-9043.00','51-2091.00','51-4191.00','51-9193.00','53-5011.00',
# '53-7032.00','53-7072.00','13-1074.00','17-3012.00','17-3012.01','17-3012.02','17-3022.00','17-3027.00','17-3027.01','19-4031.00','25-2011.00',
# '25-3021.00','27-2021.00','27-2023.00','27-3012.00','27-4021.00','29-2051.00','29-2052.00','29-2057.00','29-2071.00','29-2081.00','31-1011.00',
# '31-1014.00','31-2022.00','31-9092.00','31-9096.00','31-9097.00','33-3011.00','33-9093.00','35-1012.00','35-2012.00','37-1011.00','37-2021.00',
# '39-1011.00','39-1012.00','39-2011.00','39-6012.00','39-9011.00','39-9011.01','41-2022.00','41-2031.00','41-3041.00','43-2011.00','43-3031.00',
# '43-3041.00','43-3061.00','43-3071.00','43-4031.00','43-4031.01','43-4031.02','43-4031.03','43-4041.00','43-4041.01','43-4041.02','43-4061.00',
# '43-4121.00','43-4141.00','43-4181.00','43-5011.00','43-5011.01','43-5032.00','43-5041.00','43-5061.00','43-5071.00','43-5111.00','43-6011.00',
# '43-6012.00','43-6014.00','43-9021.00','43-9071.00','45-2011.00','45-2021.00','45-4023.00','49-2011.00','49-9062.00','49-9094.00','51-2011.00',
#                        '51-3093.00','51-6092.00','51-8021.00','51-9082.00','51-9083.00','53-3021.00','53-6061.00','53-7011.00','53-7063.00')"
# )

# All Skills from 2019 --------------------------------------
# skill <- DBI::dbGetQuery(con, "SELECT id, jobdate, skill, skillcluster, skillclusterfamily, isbaseline FROM bgt_job.skill
#                           WHERE jobdate >= '2019-01-01'
#                           AND jobdate < '2020-01-01'
#                           AND isbaseline = 'TRUE'
#                           ORDER BY id"
# )

DBI::dbDisconnect(con)

# write_rds(job, "./data/working/job.Rds")
# write_rds(skill, "./data/working/skills.Rds")

job <- read_rds("./data/working/job.Rds")
skill <- read_rds("./data/working/skill.Rds")

# Merge Jobs and Skills and Create Vectors ------------------

soc <- data.table(job, key="id")[
  data.table(skill, key="id"),
  allow.cartesian=TRUE]

soc <- soc %>% filter(onet %in% soc0) # over 17,476,416
soc1 <- soc %>% select(onet, onetname, sectorname, skill, skillcluster, skillclusterfamily) %>% unique() # 115,508
soc1 <- soc1[is.na(onet)==F,]
write_rds(soc1, "./data/working/soc_skill_long.Rds")

soc1 <- read_rds("./data/working/soc_skill_long.Rds")
soc2 <- soc1 %>% select(onet, skill) %>% unique() %>% group_by(onet) %>% mutate(number = sequence(n()))
soc2$number <- paste0("skill.", soc2$number)
soc2 <- spread(soc2, key = "number", value = "skill")

cols <- soc2 %>% ungroup() %>% select(starts_with("skill."))
cols <- names(cols)

soc2$vector <- apply( soc2[ , cols ] , 1 , paste , collapse = "," )
soc2$vector <- gsub(",NA", "", soc2$vector)
soc2$vector <- strsplit(soc2$vector, ",")
soc3 <- soc2 %>% select(onet, vector)

soc4 <- soc1 %>% select(onet, onetname) %>% unique()
socs <- left_join(soc4, soc3, by = c("onet"="onet"))
write_rds(socs, "./data/working/soc_skill.Rds")


# Failed Way --------------------
# length(unique(skill$id))
#
# data <- skill %>% select(id, skill) %>% group_by(id) %>% mutate(number = sequence(n()))
# data$number <- paste0("skill.", data$number)
# data <- spread(data, key = "number", value = "skill")
#
# cols <- data %>% ungroup() %>% select(starts_with("skill."))
# cols <- names(cols)
# data$vector <- apply( data[ , cols ] , 1 , paste , collapse = "," )
# data$vector <- gsub(",NA", "", data$vector)
# data$vector <- strsplit(data$vector, ",")
#
# data <- data %>% select(id, vector)
# rm(skill)
# write_rds(data, "./data/working/unique_skill.rds")

# data <- read_rds("./data/working/unique_skill.rds")
#
# soc <- left_join(job, data, by = c("id" = "id"))
# soc1 <- soc %>% group_by(onet) %>% mutate(count = sequence(n())) %>% top_n(100)
# soc1 %>% ggplot(aes(x=count)) + geom_histogram(bins = 100)
# soc1$number <- paste0("vector.", soc1$count)
#
# data1 <- spread(soc1, key = "number", value = "vector")

