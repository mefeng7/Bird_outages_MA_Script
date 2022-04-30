library(tidyverse)
library(patchwork)
library(officer)
library(flextable)
#remotes::install_github("rstudio/webshot2")
#webshot::install_phantomjs()

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
pred_select<-read.csv("Outputs/predictor_selection_models_updated.csv")%>%
  mutate(across(c(3:11),round,3))
  
tb2<-flextable(pred_select%>%dplyr::select(-c("Model","AICc_wt")))%>%
  set_header_labels(Name="Model of log(SAIDI)",
                    R2_conditional="R2 Conditional", 
                    R2_marginal="R2 Marginal",
                    ICC="ICC",
                    RMSE="RMSE",
                    Sigma="Sigma",
                    AIC_wt="AIC Weight",
                    BIC_wt="BIC Weight",
                    Performance_Score="Performance Score") %>%
  fontsize(size=12, part='header') %>%
  fontsize(size=11, part='body') %>%
  #font(fontname='Helvetica', part='header') %>%
  #font(fontname='Helvetica', part='body') %>%
  align(align="center",part = "all") %>%
  border_inner_h(border=fp_border(color='#CAD4D1', style='solid', width=0.5)) %>%
  bg(bg="white",part = "all")

save_as_image(tb2, "Outputs/Table2_updated.png")



species_sub<-read.csv("Outputs/species_subset_models_updated.csv")%>%
  mutate(across(c(3:11),round,3))

tb3<-flextable(species_sub%>%dplyr::select(-c("Model","AICc_wt")))%>%
  set_header_labels(Name="Model of log(SAIDI)",
                    R2_conditional="R2 Conditional", 
                    R2_marginal="R2 Marginal",
                    ICC="ICC",
                    RMSE="RMSE",
                    Sigma="Sigma",
                    AIC_wt="AIC Weight",
                    BIC_wt="BIC Weight",
                    Performance_Score="Performance Score") %>%
  fontsize(size=12, part='header') %>%
  fontsize(size=11, part='body') %>%
  #font(fontname='Helvetica', part='header') %>%
  #font(fontname='Helvetica', part='body') %>%
  align(align="center",part = "all") %>%
  border_inner_h(border=fp_border(color='#CAD4D1', style='solid', width=0.5)) %>%
  bg(bg="white",part = "all")

save_as_image(tb3, "Outputs/Table3_updated.png")


species_coef<-read.csv("Outputs/species_coefficients_updated.csv")%>%
  mutate(across(c(3:11),round,3))

tb4<-flextable(species_coef%>%dplyr::select(-c("Model","AICc_wt")))%>%
  set_header_labels(Name="Model of log(SAIDI)",
                    R2_conditional="R2 Conditional", 
                    R2_marginal="R2 Marginal",
                    ICC="ICC",
                    RMSE="RMSE",
                    Sigma="Sigma",
                    AIC_wt="AIC Weight",
                    BIC_wt="BIC Weight",
                    Performance_Score="Performance Score") %>%
  fontsize(size=12, part='header') %>%
  fontsize(size=11, part='body') %>%
  #font(fontname='Helvetica', part='header') %>%
  #font(fontname='Helvetica', part='body') %>%
  align(align="center",part = "all") %>%
  border_inner_h(border=fp_border(color='#CAD4D1', style='solid', width=0.5)) %>%
  bg(bg="white",part = "all")

save_as_image(tb4, "Outputs/Table4_updated.png")
