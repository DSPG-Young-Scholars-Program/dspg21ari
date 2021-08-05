library(tidyverse)
library(readxl)

# Skills by Variation ------------------------------------
soc_skills <- readRDS("./data/working/soc_skill_bls_long.Rds")
all_skills_soc<-readRDS("./data/working/all_soc_skill_bls_long.Rds")
crosswalk <- read_excel("./data/original/crosswalk.xlsx")

mos_skill_long <- left_join(crosswalk, soc_skills, by = c("O*NET-SOC Code" = "onet"))

skill_mos <- mos_skill_long %>% transmute(source = `Army MOS Title`, target = skill, employ = tot_emp, salary = as.numeric(a_mean)) %>%
  group_by(source, target) %>%
  mutate(n = n()) %>%
  ungroup %>%
  unique()
skill_mos <- na.omit(skill_mos)
skill_mos <- skill_mos %>% group_by(target) %>% mutate(freq = n(), employ = mean(employ), salary = mean(salary), sd=sd(n)) %>% ungroup() %>%
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

soc_skills %>%
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
       title = "Unique Baseline Skill Frequency")
