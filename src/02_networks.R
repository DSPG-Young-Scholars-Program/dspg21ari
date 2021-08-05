library(tidyverse)
library(readxl)
# library(igraph)
# library(ggraph)
# library(tidygraph)
# library(graphlayouts)
# library(visNetwork)
# library(networkD3)

#
# Network Visualizations -----------------------------
#

# http://mr.schochastics.net/netVizR.html
# https://kateto.net/network-visualization


# Visualizations
# MOS - Skill - Salary or Employment (size)
# MOS - Onetname - Employment (size)
# Data Frames to Export + Use
socs <- read_rds("./data/working/soc_skill_bls_long.Rds")
crosswalk <- read_excel("./data/original/crosswalk.xlsx")

# MOS to SOC (Long)
mos_skill_long <- left_join(crosswalk, socs, by = c("O*NET-SOC Code" = "onet"))
soc_mos <- mos_skill_long %>% transmute(source = `Army MOS Title`, target = socname, employ = tot_emp, salary = as.numeric(a_mean)) %>% unique()
soc_mos <- na.omit(soc_mos)
soc_mos <- soc_mos %>% group_by(target) %>% mutate(freq = n(), employ = mean(employ), salary = mean(salary)) %>% ungroup() %>% unique()
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

# MOS to Skill (Long)
skill_mos <- mos_skill_long %>% transmute(source = `Army MOS Title`, target = skill, employ = tot_emp, salary = as.numeric(a_mean)) %>%
  group_by(source, target) %>%
  mutate(n = n()) %>%
  ungroup %>%
  unique()
skill_mos <- na.omit(skill_mos)
skill_mos <- skill_mos %>% group_by(source, target) %>% mutate(freq2 = n()) %>% ungroup() %>%group_by(target) %>%
  mutate(freq = n(), employ = mean(employ), salary = mean(salary), sd1=sd(n), sd2 = sd(freq), sd3=sd(freq2)) %>%
  ungroup() %>%
  unique()
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

write.csv(soc_mos, "./data/working/soc_network.csv", row.names = F)
write.csv(skill_mos, "./data/working/skill_network.csv", row.names = F)

# Reduce DF -------------------------
mos_unique <- soc_mos %>%
  select(source, target) %>%
  group_by(target) %>%
  mutate(freq = n()) %>%
  filter(freq < 10)

skill_unique <- skill_mos %>%
  select(source, target) %>%
  group_by(target) %>%
  mutate(freq = n()) %>%
  filter(freq < 10)

soc_mos_r <- soc_mos %>%
  arrange(desc(e_freq)) %>%
  top_frac(.5, e_freq) # 46 jobs
write.csv(soc_mos_r, "./data/working/soc_network_reduce.csv", row.names = F)

skill_mos_r <- skill_mos %>%
  arrange(desc(e_freq)) %>%
  top_frac(.5, e_freq) # 52 skills
write.csv(skill_mos_r, "./data/working/skill_network_reduce.csv", row.names = F)

soc_mos_unique <- soc_mos %>%
  filter(target %in% mos_unique$target) %>%
  arrange(desc(e_freq)) %>%
  top_frac(.5, e_freq)
write.csv(soc_mos_unique, "./data/working/soc_network_unique.csv", row.names = F)

skill_mos_unique <- skill_mos %>%
  filter(target %in% skill_unique$target) %>%
  arrange(desc(e_freq))
write.csv(skill_mos_unique, "./data/working/skill_network_unique.csv", row.names = F)

# Network DF - All Skills ---------------------------------------
# Data Frames to Export + Use
all_long <- read_rds("/sfs/qumulo/qhome/mes5bu/git/ari3/data/working/all_soc_skill_bls_long.Rds")

# MOS to all skill (Long)
mos_skill_long <- left_join(crosswalk, all_long, by = c("O*NET-SOC Code" = "onet"))
mos_skill_long$skill_type <- ifelse(mos_skill_long$isbaseline == T, "Baseline",
                                    ifelse(mos_skill_long$issoftware == T, "Software", "Specialized"))
skill_mos <- mos_skill_long %>% transmute(source = `Army MOS Title`, target = skill, employ = tot_emp, salary = as.numeric(a_mean), skill_type) %>%
  group_by(source, target) %>%
  mutate(n = n()) %>%
  ungroup %>%
  unique()
skill_mos <- na.omit(skill_mos)
skill_mos <- skill_mos %>% group_by(source, target) %>% mutate(freq2 = n()) %>% ungroup() %>% group_by(target) %>%
  mutate(freq = n(), employ = mean(employ), salary = mean(salary), sd1=sd(n), sd2 = sd(freq)) %>%
  ungroup() %>% unique()
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
write.csv(skill_mos, "./data/working/all_skill_mos_network.csv", row.names = F)

# Reduce DF -------------------------
skill_unique_all <- skill_mos %>%
  select(source, target) %>%
  group_by(target) %>%
  mutate(freq = n()) %>%
  filter(freq < 10)

skill_mos_r <- skill_mos %>%
  arrange(desc(e_freq)) %>%
  top_frac(.02, e_freq) # 897 skills
write.csv(skill_mos_r, "./data/working/all_skill_reduce.csv", row.names = F)

skill_mos_unique <- skill_mos %>%
  filter(target %in% skill_unique_all$target) %>%
  arrange(desc(e_freq)) %>%
  top_frac(.05, e_freq) # 897 skills
write.csv(skill_mos_unique, "./data/working/all_skill_unique.csv", row.names = F)


# Networks in R -----------------
# https://www.jessesadler.com/post/network-analysis-with-r/

# iGraph
# mos <- skill_mos %>%
#   distinct(source) %>%
#   rename(label = source)
# skills <- skill_mos %>%
#   distinct(target) %>%
#   rename(label = target)
# nodes <- full_join(mos, skills, by = "label")
#
# skill_network <- graph_from_data_frame(d = skill_mos, vertices = nodes, directed = TRUE)
# plot(skill_network, edge.arrow.size = 0.2)
# plot(skill_network, layout = layout_with_graphopt, edge.arrow.size = 0.2)
#
# # Tidygraph
# skill_tidy <- as_tbl_graph(skill_network)
#
# ggraph(skill_tidy, layout = "graphopt") +
#   geom_node_point() +
#   geom_edge_link(aes(width = e_freq), alpha = 0.8) +
#   scale_edge_width(range = c(0.2, 2)) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   labs(edge_width = "Employment\n Weighted\n Frequency") +
#   theme_graph()
#
# # VisNetwork
# nodes2 <- nodes %>% rename(id = label)
# edges2 <- skill_mos %>% transmute(from = source, to = target, weight = e_freq)
#
# visNetwork(nodes2, edges2)
# visIgraph(skill_network)
# visNetwork(nodes2, edges2) %>%
#   visIgraphLayout(layout = "layout_with_fr") %>%
#   visEdges(arrows = "middle")
#
# # D3
# nodesd3 <- as.data.frame(nodes2)
# edgesd3 <- as.data.frame(edges2)
#
# forceNetwork(Links = edgesd3, Nodes = nodesd3, Source = "from", Target = "to",
#              NodeID = "id", Group = "id", Value = "weight",
#              opacity = 1, fontSize = 16, zoom = TRUE)
