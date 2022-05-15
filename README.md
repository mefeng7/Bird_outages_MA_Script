This contains the script for an analysis of animal-related outages in the state of Massachusetts from 2013-2018.

All data formating and analysis took place in R version 4.0.3.

The original outage data comes from Eversource Energy, National Grid, and Unitil Corporation, made available through the MA office of Energy and Environmental Affairs. The outage dataset used in this analysis is available on Columbia University's International Research Institute (IRI) for Climate and Society Data Library at http://iridl.ldeo.columbia.edu/SOURCES/.EOEEA/.

The original bird abundance data comes from the eBird Basic Dataset (May 2020) and the modeled relative abundance estimates for Massachusetts towns are also available on the IRI Data Library at http://iridl.ldeo.columbia.edu/SOURCES/.PRISM/.eBird/.derived/.detectionProbability/.

To download the accompanying data for this analysis, visit our data repository on figshare (https://figshare.com/articles/dataset/Bird_outages_MA_data/19602142). Unzip and add the "Objects and Data" and "Outputs" folders to the repository folder for this analysis script (Bird_outages_MA_script).

Additional environmental data needed for the analysis in 1_Habitat_Data_Preparation.R must be downloaded separately from the National Land Cover Database and Amatulli et al 2018 and added to Objects and Data. See 1_Habitat_Data_Preparation.R for more details on the downloads needed.

To run the analysis and generate figures, run the 0_Main.R script. 0_Main.R sources the scripts for each part of the analysis and figure generation.

Please cite the outage data, bird data, and figures generated in this analysis as the following: Feng, M.-L.E., Owolabi, O.O., Schafer, L.J., Sengupta, S., Wang, L., Matteson, D.S., Che-Castaldo, J.P., and Sunter, D.A. 2022. Analysis of animal-related electric outages using species distribution models and community science data. Environmental Research: Ecology. [Manuscript under Review].
