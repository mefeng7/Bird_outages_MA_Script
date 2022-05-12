library(tidyverse)
library(ggcorrplot)
library(AICcmodavg)
library(regclass)
library(lme4)
library(performance)


load("Objects and Data/4_dp_outs_towns_updated.rda")
load("Objects and Data/0_species_list.rda")
load("Objects and Data/5_DP_PCA_updated.rda")

dp<-dp_outs_towns%>%
  left_join(coord[,c("city","Year","week","Dim.1","Dim.2")], 
            by = c("actual_city_town"="city","week"="week","year"="Year"))%>%
  #remove missing data
  filter(saidi!=0 & !(is.na(RWBL)))%>%
    mutate(week=as.factor(week),
           year=as.factor(year),
          month=as.factor(month))%>%
  dplyr::select(-c("Cultivated_Crops", "Shrub_Scrub","county.x","county.y","date","week"))%>%
  #add indicators of spatio-temporal subsets of the data
  #Forested areas, Developed areas, Summer season, Winter Season
  mutate(Developed.ind=ifelse(Developed>=quantile(Developed,0.75),1,0),
         Forest.ind=ifelse(Forest>=quantile(Forest,0.75),1,0),
         Summer.ind=ifelse(season=="summer",1,0),
         Winter.ind=ifelse(season=="winter",1,0))%>%
  #Select representative species of each spatio-temporal trend
  mutate(Migrant=RWBL,
         Resident=RBWO,#Also no spatial trend group
         Forest.sp=PIWO,
         Urban.sp=EUST)#Also in the no temporal trend group
  

#-------------------------------------------------------------------------------------------
#Check for correlation and collinearity between predictors
#1. Between species
cor1<-round(cor(dp[,toupper(name_list$sp_file[-16])]),1)
cor1p<-round(cor_pmat(dp[,toupper(name_list$sp_file[-16])]),4)
ggcorrplot(cor1,type = "lower", outline.color = "white",hc.order = T, 
           p.mat=cor1p,insig="blank",
           lab=T)

#blackbird species correlate and woodpecker species correlate. 
#Select representative species.RWBL; RBWO,NOFL,PIWO
#Remove COGR, BHCO, DOWO, HAWO

#2. Between species and habitat
cor2<-round(cor(dp[,c(toupper(name_list$sp_file[-16]),
                      "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
            ,1)
cor2p<-round(cor_pmat(dp[,c(toupper(name_list$sp_file[-16]),
                            "Forest","Developed","Grassland", "Open_Water", "Barren_Land")])
             ,4)
ggcorrplot(cor2,type = "lower", outline.color = "white", 
           p.mat=cor2p,insig="blank",
           lab=T)

#PIWO correlated with forested habitat, NOFL with developed habitat
#HOSP correlated with developed


#3. Variance inflation factor
#smallest value of VIF is 1 = no collinearity. Exceeds 5 or 10 = collinearity.

m<-lm(log_saidi~
        TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+NOFL+EUST+AMCR+
        month+year+
        Developed+Forest+Barren_Land+Open_Water+Grassland,data=dp)
VIF(m)
m2<-lm(log_saidi~
         TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+NOFL+EUST+AMCR+
         month+year+
         Forest+Barren_Land+Open_Water+Grassland,data=dp)
VIF(m2)

#Removing Developed/Forest habitat reduces multicollinearity




#----------------------------------------------------------------------------------------------------------------------------------

#Question 1: Does the inclusion of species activity improve SAIDI model performance over using only spatio-temporal trends?
#############################################################################################################################

#Create a set of models to compare performance


# 1. habitat and time
lm.th<-lm(log_saidi~year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.th<-lmer(log_saidi~month+#year+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.th)


#2. Just habitat
lm.h<-lm(log_saidi~Barren_Land+Open_Water+Grassland+Forest,
          data=dp)

lmer.h<-lmer(log_saidi~Barren_Land+Open_Water+Grassland+Forest+
                (1|actual_city_town)+(1|year),
              data=dp)

summary(lm.h)


# 3. Just time
lm.t<-lm(log_saidi~year+month,
          data=dp)

lmer.t<-lmer(log_saidi~month+#year+
                (1|actual_city_town)+(1|year),
              data=dp)

summary(lm.t)


#4. Just species
lm.s<-lm(log_saidi~TUVU+MODO+HOSP+
             OSPR+RTHA+RWBL+
             PIWO+RBWO+NOFL+
             EUST+AMCR,
           data=dp)

lmer.s<-lmer(log_saidi~TUVU+MODO+HOSP+
                 OSPR+RTHA+RWBL+
                 PIWO+RBWO+NOFL+
                 EUST+AMCR+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.s)



# 5. species, habitat, and time (Additive)
lm.sth.a<-lm(log_saidi~TUVU+MODO+HOSP+
             OSPR+RTHA+RWBL+
             PIWO+RBWO+NOFL+
             EUST+AMCR+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.sth.a<-lmer(log_saidi~TUVU+MODO+HOSP+
                 OSPR+RTHA+RWBL+
                 PIWO+RBWO+NOFL+
                 EUST+AMCR+
                 #year+
                   month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town)+(1|year),
                 data=dp)

summary(lmer.sth.a)


# 6. species, habitat, and time (Interactions)
lm.sth.i<-lm(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+
             (OSPR*Forest)+(RTHA*Forest)+(RWBL*Forest)+
             (PIWO*Forest)+(RBWO*Forest)+(NOFL*Forest)+
             (EUST*Forest)+(AMCR*Forest)+
             (TUVU*month)+(MODO*month)+(HOSP*month)+
             (OSPR*month)+(RTHA*month)+(RWBL*month)+
             (PIWO*month)+(RBWO*month)+(NOFL*month)+
             (EUST*month)+(AMCR*month)+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.sth.i<-lmer(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+
                 (OSPR*Forest)+(RTHA*Forest)+(RWBL*Forest)+
                 (PIWO*Forest)+(RBWO*Forest)+(NOFL*Forest)+
                 (EUST*Forest)+(AMCR*Forest)+
                 (TUVU*month)+(MODO*month)+(HOSP*month)+
                 (OSPR*month)+(RTHA*month)+(RWBL*month)+
                 (PIWO*month)+(RBWO*month)+(NOFL*month)+
                 (EUST*month)+(AMCR*month)+
                 #year+
                   month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.sth.i)




# 7. Just species PCs
lm.pc<-lm(log_saidi~Dim.1+Dim.2,
            data=dp)

lmer.pc<-lmer(log_saidi~Dim.1+Dim.2+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.pc)


# 8. species PCs, habitat, and time (Additive)
lm.pcth.a<-lm(log_saidi~Dim.1+Dim.2+
              year+month+
              Barren_Land+Open_Water+Grassland+Forest,
            data=dp)

lmer.pcth.a<-lmer(log_saidi~Dim.1+Dim.2+
                  #year+
                    month+
                  Barren_Land+Open_Water+Grassland+Forest+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.pcth.a)

# 9. species PCs, habitat, and time (Interactions)
lm.pcth.i<-lm(log_saidi~(Dim.1*Forest)+(Dim.2*Forest)+(Dim.1*month)+(Dim.2*month)+
              year+month+
              Barren_Land+Open_Water+Grassland+Forest,
            data=dp)

lmer.pcth.i<-lmer(log_saidi~(Dim.1*Forest)+(Dim.2*Forest)+(Dim.1*month)+(Dim.2*month)+
                  #year+
                    month+
                  Barren_Land+Open_Water+Grassland+Forest+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.pcth.i)


  # 10. representative species from each PC trend (resident, migrant, forest, and urban)
lm.rep<-lm(log_saidi~Migrant+Resident+Forest.sp+Urban.sp,
            data=dp)

lmer.rep<-lmer(log_saidi~Migrant+Resident+Forest.sp+Urban.sp+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.rep)


# 11. representative species, habitat, and time (Additive)
lm.repth.a<-lm(log_saidi~Migrant+Resident+Forest.sp+Urban.sp+
              year+month+
              Barren_Land+Open_Water+Grassland+Forest,
            data=dp)

lmer.repth.a<-lmer(log_saidi~Migrant+Resident+Forest.sp+Urban.sp+
                  #year+
                    month+
                  Barren_Land+Open_Water+Grassland+Forest+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.repth.a)


# 12. representative species, habitat, and time (Interactions)
lm.repth.i<-lm(log_saidi~(Migrant*Forest)+(Resident*Forest)+(Forest.sp*Forest)+(Urban.sp*Forest)+
              (Migrant*month)+(Resident*month)+(Forest.sp*month)+(Urban.sp*month)+
              year+month+
              Barren_Land+Open_Water+Grassland+Forest,
            data=dp)

lmer.repth.i<-lmer(log_saidi~(Migrant*Forest)+(Resident*Forest)+(Forest.sp*Forest)+(Urban.sp*Forest)+
                  (Migrant*month)+(Resident*month)+(Forest.sp*month)+(Urban.sp*month)+
                  #year+
                    month+
                  Barren_Land+Open_Water+Grassland+Forest+
                  (1|actual_city_town)+(1|year),
                data=dp)

summary(lm.repth.i)



#Question 2: Do diverse species activity patterns improve SAIDI model performance over using only single species patterns?
#############################################################################################################################

#Urban spatial trend
#######################
# 13. representative species from Urban PC trend
lm.u<-lm(log_saidi~Urban.sp,
           data=dp)

lmer.u<-lmer(log_saidi~Urban.sp+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.u)

# 14. representative species from Urban PC trend, habitat and time (additive)
lm.uth.a<-lm(log_saidi~Urban.sp+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.uth.a<-lmer(log_saidi~Urban.sp+
                 #year+
                   month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.uth.a)

# 15. representative species from Urban PC trend, habitat and time (interaction)
lm.uth.i<-lm(log_saidi~(Urban.sp*Forest)+(Urban.sp*month)+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.uth.i<-lmer(log_saidi~(Urban.sp*Forest)+(Urban.sp*month)+
                 #year+
                   month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.uth.i)



#Resident temporal trend
###############################
# 16. representative species from Resident PC trend
lm.res<-lm(log_saidi~Resident,
           data=dp)

lmer.res<-lmer(log_saidi~Resident+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.res)

# 17. representative species from Resident PC trend, habitat and time (additive)
lm.resth.a<-lm(log_saidi~Resident+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.resth.a<-lmer(log_saidi~Resident+
                   #year+
                     month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.resth.a)

# 18. representative species from Resident PC trend, habitat and time (interaction)
lm.resth.i<-lm(log_saidi~(Resident*Forest)+(Resident*month)+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.resth.i<-lmer(log_saidi~(Resident*Forest)+(Resident*month)+
                   #year+
                     month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.resth.i)


#Migratory temporal trend
###########################
# 19. representative species from Migrant PC trend
lm.m<-lm(log_saidi~Migrant,
           data=dp)

lmer.m<-lmer(log_saidi~Migrant+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lmer.m)


# 20. representative species from Urban PC trend, habitat and time (additive)
lm.mth.a<-lm(log_saidi~Migrant+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.mth.a<-lmer(log_saidi~Migrant+
                   #year+
                   month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.mth.a)

# 21. representative species from Migrant PC trend, habitat and time (interaction)
lm.mth.i<-lm(log_saidi~(Migrant*Forest)+(Migrant*month)+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.mth.i<-lmer(log_saidi~(Migrant*Forest)+(Migrant*month)+
                   #year+
                   month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.mth.i)


#Forest spatial trend
#######################
# 22. representative species from Forest PC trend
lm.f<-lm(log_saidi~Forest.sp,
           data=dp)

lmer.f<-lmer(log_saidi~Forest.sp+
                 (1|actual_city_town)+(1|year),
               data=dp)

summary(lm.f)


# 23. representative species from Forest PC trend, habitat and time (additive)
lm.fth.a<-lm(log_saidi~Forest.sp+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.fth.a<-lmer(log_saidi~Forest.sp+
                  # year+
                   month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.fth.a)

# 24. representative species from Forest PC trend, habitat and time (interaction)
lm.fth.i<-lm(log_saidi~(Forest.sp*Forest)+(Forest.sp*month)+
               year+month+
               Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.fth.i<-lmer(log_saidi~(Forest.sp*Forest)+(Forest.sp*month)+
                   #year+
                   month+
                   Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town)+(1|year),
                 data=dp)

summary(lm.fth.i)



#Use a mixed model to account for random effect of town?

anova(lmer.sth.i,lm.sth.i)
anova(lmer.th,lm.th)
anova(lmer.fth.i,lm.fth.i)
anova(lmer.uth.i,lm.uth.i)
anova(lmer.resth.i,lm.resth.i)
anova(lmer.mth.i,lm.mth.i)
anova(lmer.pcth.i,lm.pcth.i)
anova(lmer.repth.i,lm.repth.i)

anova(lmer.sth.a,lm.sth.a)
anova(lmer.th,lm.th)
anova(lmer.fth.a,lm.fth.a)
anova(lmer.uth.a,lm.uth.a)
anova(lmer.resth.a,lm.resth.a)
anova(lmer.mth.a,lm.mth.a)
anova(lmer.pcth.a,lm.pcth.a)
anova(lmer.repth.a,lm.repth.a)

anova(lmer.s,lm.s)
anova(lmer.t,lm.t)
anova(lmer.h,lm.h)
anova(lmer.f,lm.f)
anova(lmer.u,lm.u)
anova(lmer.res,lm.res)
anova(lmer.m,lm.m)
anova(lmer.pc,lm.pc)
anova(lmer.rep,lm.rep)
#Random effect is significant



#Compare model performance between models 
models <- list(lmer.sth.i,lmer.th,lmer.fth.i,
               lmer.uth.i,lmer.resth.i,lmer.mth.i,
               lmer.pcth.i,lmer.repth.i,lmer.sth.a,
               lmer.fth.a,lmer.uth.a,
               lmer.resth.a,lmer.mth.a,lmer.pcth.a,
               lmer.repth.a,lmer.s,lmer.t,
               lmer.h,lmer.f,lmer.u,lmer.res,
               lmer.m,lmer.pc,lmer.rep)

names(models)<-c('All Species, Habitat, and Time (Interaction)','Time and Habitat','Forest Species, Habitat, and Time (Interaction)',
                 'Urban Species, Habitat, and Time (Interaction)','Resident Species, Habitat, and Time (Interaction)','Migrant Species, Habitat, and Time (Interaction)',
                 'All PC, Habitat, and Time (Interaction)','All Representative Species, Habitat, and Time (Interaction)','All Species, Habitat, and Time (Additive)',
                  'Forest Species, Habitat, and Time (Additive)',
                 'Urban Species, Habitat, and Time (Additive)','Resident Species, Habitat, and Time (Additive)','Migrant Species, Habitat, and Time (Additive)',
                 'All PC, Habitat, and Time (Additive)','All Representative Species, Habitat, and Time (Additive)','All Species',
                 'Time',"Habitat","Forest Species","Urban Species","Resident Species","Migrant Species","All PC","All Representative Species")


var_compare<-compare_performance(models,rank=T)
#add the number of coefficients in each model
Fixed.Effects<-data.frame(Name=names(models),Fixed.Effects=unlist(lapply(models,function(x){length(fixef(x))})))
var_compare<-left_join(var_compare,Fixed.Effects,by="Name")
  #Focus on R squared's, BIC, and number of parameters for perfomance metrics

write.csv(var_compare,"Outputs/predictor_selection_models_updated.csv",row.names = F)




#Question 3: Do specific species contribute more to AROs in different habitats and seasons?
#############################################################################################################################

#Fit 1 model, 
#make indicator of observations above 75th quantiles for Forest and developed land cover
#make indicator of observations within summer and winter seasons
#report species interactions with each indicator

#summary table is coeff for each species, and then the interaction for each indicator (add species coeff to interaction coeff)
#Don't use model ranking, just compare variance explained R2.

indicators<-lmer(log_saidi~(TUVU*Summer.ind)+(TUVU*Winter.ind)+(TUVU*Forest.ind)+(TUVU*Developed.ind)+
                   (MODO*Summer.ind)+(MODO*Winter.ind)+(MODO*Forest.ind)+(MODO*Developed.ind)+
                   (HOSP*Summer.ind)+(HOSP*Winter.ind)+(HOSP*Forest.ind)+(HOSP*Developed.ind)+
                 (OSPR*Summer.ind)+(OSPR*Winter.ind)+(OSPR*Forest.ind)+(OSPR*Developed.ind)+
                   (RTHA*Summer.ind)+(RTHA*Winter.ind)+(RTHA*Forest.ind)+(RTHA*Developed.ind)+
                   (RWBL*Summer.ind)+(RWBL*Winter.ind)+(RWBL*Forest.ind)+(RWBL*Developed.ind)+
                 (PIWO*Summer.ind)+(PIWO*Winter.ind)+(PIWO*Forest.ind)+(PIWO*Developed.ind)+
                   (RBWO*Summer.ind)+(RBWO*Winter.ind)+(RBWO*Forest.ind)+(RBWO*Developed.ind)+
                   (NOFL*Summer.ind)+(NOFL*Winter.ind)+(NOFL*Forest.ind)+(NOFL*Developed.ind)+
                 (EUST*Summer.ind)+(EUST*Winter.ind)+(EUST*Forest.ind)+(EUST*Developed.ind)+
                   (AMCR*Summer.ind)+(AMCR*Winter.ind)+(AMCR*Forest.ind)+(AMCR*Developed.ind)+
                 year+
                 month+
                 Barren_Land+Open_Water+Grassland+Forest+
                   (1|actual_city_town),
               data=dp)

summary(indicators)
#Make table of significant species interaction coefficients

  
  coefs <- as.data.frame(round(coef(summary(indicators)),3))%>%
  filter(abs(`t value`)>1.98)

  
