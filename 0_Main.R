library(tidyverse)
library(patchwork)

memory.limit(100000)

#---------------------------------------------------------------------------------------------------
# source analysis script

#preparing land cover and elevation variables for species models
source("1_Habitat_Data_Preparation.R")

#Species distribution models, predict Detection Probability for each species
source("2_Bird_DP_Model.R")

#Merge species DPs and outage data in Massachusetts
source("3_Outage_birdDP_merge.R")

#Principal Component Analysis of bird DPs
source("4_BirdDP_PCA.R")

#Multiple linear regression comparing important variables for predicting SAIDI
source("5_Regression.R")

#-----------------------------------------------------------------------------------------
# source figure script
source("Fig_timeseries.R")
source("Fig_maps.R")
source("Fig_pc_loadings.R")

#---------------------------------------------------------------------------------------------------
# plot and save / = below | = beside

p1
ggsave("Outputs/Figures/SpeciesMaps_group1_updated.jpeg", width = 6, height = 4, dpi = "retina")

p2
ggsave("Outputs/Figures/SpeciesMaps_group2_updated.jpeg", width = 6, height = 6, dpi = "retina")

p3
ggsave("Outputs/Figures/SpeciesMaps_group3_updated.jpeg", width = 6, height = 8, dpi = "retina")

lc_map
ggsave("Outputs/Figures/MA_lc_map_updated.jpeg", width = 5, height = 5, dpi = "retina")

p_map/lc_map + plot_annotation(tag_levels = 'A')+ 
  plot_layout(heights = unit(c(6, 5), c('cm', 'null')))
ggsave("Outputs/Figures/SpeciesMaps_contrast_updated.jpeg", width = 6, height = 6, dpi = "retina")

g4 <- (p4 / p5 / p6) 
g4
ggsave("Outputs/Figures/SpeciesTS_groups_updated.jpeg", width = 6, height = 6.5, dpi = "retina")

p7
ggsave("Outputs/Figures/PC1_PC2_loadings_updated.jpeg", width = 6, height = 6, dpi = "retina")

