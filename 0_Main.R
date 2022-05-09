library(tidyverse)
library(patchwork)
library(xtable)

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
# plot and save figures / = below | = beside

p1
ggsave("Outputs/Figures/SpeciesMaps_group1_updated.jpeg", width = 6, height = 4, dpi = "retina")

p2
ggsave("Outputs/Figures/SpeciesMaps_group2_updated.jpeg", width = 4, height = 6, dpi = "retina")

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


#---------------------------------------------------------------------------------------------------
# plot and save tables

#Table 2.
pred_select<-read.csv("Outputs/predictor_selection_models_updated.csv")%>%
  dplyr::select(lnSAIDI_Model=Name, R2_conditional, ICC, BIC_Weight=BIC_wt,Fixed_Effects=Fixed.Effects)%>%
  mutate(across(-c("lnSAIDI_Model","Fixed_Effects"),round,4),
         Fixed_Effects=as.character(Fixed_Effects),
         Model_Hypothesis=case_when(
           lnSAIDI_Model%in%c("Time and Habitat","Habitat","Time")~"Null Model (No Species)",
           lnSAIDI_Model%in%c("Resident Species","Migrant Species","Urban Species", "Forest Species",
                              'Forest Species, Habitat, and Time (Interaction)',
                              'Urban Species, Habitat, and Time (Interaction)',
                              'Resident Species, Habitat, and Time (Interaction)',
                              'Migrant Species, Habitat, and Time (Interaction)',
                              'Forest Species, Habitat, and Time (Additive)',
                              'Urban Species, Habitat, and Time (Additive)',
                              'Resident Species, Habitat, and Time (Additive)',
                              'Migrant Species, Habitat, and Time (Additive)')~"Single Species Model",
           lnSAIDI_Model%in%c("All Representative Species","All PC","All Species",
                              'All PC, Habitat, and Time (Additive)',
                              'All Representative Species, Habitat, and Time (Additive)',
                              'All Species, Habitat, and Time (Additive)',
                              'All Species, Habitat, and Time (Interaction)',
                              'All PC, Habitat, and Time (Interaction)',
                              'All Representative Species, Habitat, and Time (Interaction)')~"Multi-Species Model")
         )%>%
  arrange(desc(BIC_Weight))
pred_select<-pred_select[c(6,1,2,3,4,5)]
  

#Print for latex
print(xtable(pred_select, label='tab:Table2', digits=4), type="latex")
  

c('All Species, Habitat, and Time (Interaction)','Time and Habitat','Forest Species, Habitat, and Time (Interaction)',
  'Urban Species, Habitat, and Time (Interaction)','Resident Species, Habitat, and Time (Interaction)','Migrant Species, Habitat, and Time (Interaction)',
  'All PC, Habitat, and Time (Interaction)','All Representative Species, Habitat, and Time (Interaction)','All Species, Habitat, and Time (Additive)',
  'Forest Species, Habitat, and Time (Additive)',
  'Urban Species, Habitat, and Time (Additive)','Resident Species, Habitat, and Time (Additive)','Migrant Species, Habitat, and Time (Additive)',
  'All PC, Habitat, and Time (Additive)','All Representative Species, Habitat, and Time (Additive)','All Species',
  'Time',"Habitat","Forest Species","Urban Species","Resident Species","Migrant Species","All PC","All Representative Species")


