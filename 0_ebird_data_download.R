#install.packages("remotes")
#remotes::install_github("mstrimas/ebppackages")

library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)

#example data downloaded from eBird Basic Dataset
#ebd_AMCR <- auk_ebd("Objects and Data/eBird/ebd_US-MA_amecro_200501_201812_relJan-2022.txt", 
#               file_sampling = "Objects and Data/eBird/ebd_sampling_relJan-2022.txt")
#ebd_MOPA <- auk_ebd("Objects and Data/eBird/ebd_US-MA_monpar_200501_201812_relJan-2022.txt", 
 #              file_sampling = "Objects and Data/eBird/ebd_sampling_relJan-2022.txt")

ebd_filt <- ebd_AMCR %>% 
  auk_species("American Crow") %>% 
  # State of MASS
  auk_state(state = "US-MA") %>% 
  # june, use * to get data from any year
  auk_date(date = c("2005-01-01", "2018-12-31")) %>% 
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling","Random")) %>% 
  auk_complete()


# output files
f_ebd_AMCR <- file.path("Objects and Data/eBird/ebd_AMCR_june_bcr27.txt")
f_ebd_MOPA <- file.path("Objects and Data/eBird/ebd_MOPA_june_bcr27.txt")
f_sampling <- file.path("Objects and Data/eBird/ebd_checklists_05_18_MA.txt")

# only run if the files don't already exist
  auk_filter(ebd_filt, file = f_ebd_AMCR, file_sampling = f_sampling)
  auk_filter(ebd_filt, file = f_ebd_MOPA, file_sampling = f_sampling)
  auk_filter(ebd_filt, file = f_ebd_MOPA, file_sampling = f_sampling_all)
  
#create list lengths from sample data, Use the all species EBD and Sampling files, group and count species by checklist_id
LLA<-read.csv("Objects and Data/LLA.csv")
  

#zero-fill filtered species data 
  ebd_zf_AMCR <- auk_zerofill(f_ebd_AMCR, f_sampling, collapse = TRUE)
  ebd_zf_MOPA <- auk_zerofill(f_ebd_MOPA, f_sampling, collapse = TRUE)
  
#Non-zero filled, filtered lists
 ebd_AMCR<-read_ebd(f_ebd_AMCR) 
 ebd_MOPA<-read_ebd(f_ebd_MOPA) 

#add to species_zf in 1_mass_bird_data_2018.rda (pulled in from Feng and Che-Castaldo 2021, PLOSONE)
  load("Objects and Data/1_mass_bird_data_2018.rda")
  
species_filt<-c(species_filt, list(ebd_AMCR, ebd_MOPA))
species_zf<-c(species_zf, list(ebd_zf_AMCR, ebd_zf_MOPA))
save(species_filt,species_zf,species_list,file="Objects and Data/1_mass_bird_data_2018.rda")  
