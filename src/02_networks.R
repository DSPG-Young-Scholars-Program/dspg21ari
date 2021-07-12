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
