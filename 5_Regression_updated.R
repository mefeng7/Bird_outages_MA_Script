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
  mutate(Developed=ifelse(Developed>=quantile(Developed,0.75),1,0),
         Forest=ifelse(Forest>=quantile(Forest,0.75),1,0),
         Summer=ifelse(season=="summer",1,0),
         Winter=ifelse(season=="winter",1,0))%>%
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

lmer.th<-lmer(log_saidi~year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lm.th)



  # 2. all species, habitat, and time
lm.sth<-lm(log_saidi~TUVU+MODO+HOSP+
             OSPR+RTHA+RWBL+
             PIWO+RBWO+NOFL+
             EUST+AMCR+TUVU+
             MODO+HOSP+OSPR+
             RTHA+RWBL+PIWO+
             RBWO+NOFL+EUST+AMCR+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
             data=dp)

lmer.sth<-lmer(log_saidi~TUVU+MODO+HOSP+
                 OSPR+RTHA+RWBL+
                 PIWO+RBWO+NOFL+
                 EUST+AMCR+TUVU+
                 MODO+HOSP+OSPR+
                 RTHA+RWBL+PIWO+
                 RBWO+NOFL+EUST+AMCR+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
                 data=dp)

summary(lm.sth)



  # 3. species PCs, habitat, and time 
lm.pcth<-lm(log_saidi~Dim.1+Dim.2+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.pcth<-lmer(log_saidi~Dim.1+Dim.2+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lm.pcth)


#Question 2: Do diverse species activity patterns improve SAIDI model performance over using only single species patterns?
#############################################################################################################################

  # 4. representative species from each PC trend (resident, migrant, forest, and urban)
lm.repth<-lm(log_saidi~Migrant+Resident+Forest.sp+Urban.sp+
              year+month+
              Barren_Land+Open_Water+Grassland+Forest,
            data=dp)

lmer.repth<-lmer(log_saidi~Migrant+Resident+Forest.sp+Urban.sp+
                  year+month+
                  Barren_Land+Open_Water+Grassland+Forest+
                  (1|actual_city_town),
                data=dp)

summary(lm.repth)


# 5. representative species from Urban PC trend
lm.uth<-lm(log_saidi~Urban.sp+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.uth<-lmer(log_saidi~Urban.sp+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lm.uth)


# 6. representative species from Resident PC trend
lm.resth<-lm(log_saidi~Resident+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.resth<-lmer(log_saidi~Resident+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lm.resth)


# 7. representative species from Migrant PC trend
lm.mth<-lm(log_saidi~Migrant+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.mth<-lmer(log_saidi~Migrant+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lmer.sth)


# 8. representative species from Forest PC trend
lm.fth<-lm(log_saidi~Forest.sp+
             year+month+
             Barren_Land+Open_Water+Grassland+Forest,
           data=dp)

lmer.fth<-lmer(log_saidi~Forest.sp+
                 year+month+
                 Barren_Land+Open_Water+Grassland+Forest+
                 (1|actual_city_town),
               data=dp)

summary(lm.fth)



#Use a mixed model to account for random effect of town?

anova(lmer.sth,lm.sth)
anova(lmer.th,lm.th)
anova(lmer.fth,lm.fth)
anova(lmer.uth,lm.uth)
anova(lmer.resth,lm.resth)
anova(lmer.mth,lm.mth)
anova(lmer.pcth,lm.pcth)
anova(lmer.repth,lm.repth)
#Random effect is significant



#Compare model performance between models 
models <- list(lmer.sth, lmer.th, lmer.fth,
               lmer.uth, lmer.resth, lmer.mth,
               lmer.pcth,lmer.repth)

names(models)<-c('All Species','Time and Habitat','Forest Species',
                 'Urban Species','Resident Species','Migrant Species',
                 'All PC','All Representative Species')


var_compare<-compare_performance(models,rank=T)
#add the number of coefficients in each model
Fixed.Effects<-data.frame(Name=names(models),Fixed.Effects=unlist(lapply(models,function(x){length(fixef(x))})))
var_compare<-left_join(var_compare,Fixed.Effects,by="Name")
  #Focus on R squared's, BIC, and number of parameters for perfomance metrics

write.csv(var_compare,"Outputs/predictor_selection_models_updated.csv",row.names = F)




#Question 3: Does the inclusion of species activity improve SAIDI model performance over using only spatio-temporal trends?
#############################################################################################################################

#Fit 1 model, 
#make indicator of observations above 75th quantile for habitat
#make indicator of observation just within summer and winter
#use interactions with each indicator

#summary table is coeff for each species, and then the interaction for each indicator (add species coeff to interaction coeff)

##Don't use model ranking, just compare variance explained R2.
#3. Compare species*habitat*time models using subsets of data in each season (remove month)
#and in forest vs developed+barren habitat types (remove habitat)
m.summer2<-lmer(log_saidi~TUVU+(MODO*month)+HOSP+OSPR+(RTHA*month)+
                  (RWBL*month)+(PIWO*Forest)+RBWO+(NOFL*month)+EUST+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(season=="summer"))
m.winter2<-lmer(log_saidi~TUVU+(MODO*month)+HOSP+OSPR+(RTHA*month)+
                  (RWBL*month)+(PIWO*Forest)+RBWO+(NOFL*month)+EUST+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(season=="winter"))
m.Forest2<-lmer(log_saidi~TUVU+(MODO*season)+HOSP+OSPR+(RTHA*season)+
                  (RWBL*season)+(PIWO*Forest)+RBWO+(NOFL*season)+EUST+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(Forest>quantile(Forest,0.75)))
m.Developed2<-lmer(log_saidi~TUVU+(MODO*season)+HOSP+OSPR+(RTHA*season)+
                     (RWBL*season)+(PIWO*Forest)+RBWO+(NOFL*season)+EUST+AMCR+year+
                     Barren_Land+(1|actual_city_town),data=dp%>%filter(Developed>quantile(Developed,0.75)))


#Compare model performance in a table
#define list of models
models3 <- list(m.sth2,m.summer2,m.winter2,m.Forest2, m.Developed2)

#specify model names
names(models3) <- c('All.Space.Time',
                    'Summer', 
                    'Winter', 
                    'Forested',
                    'Developed')

scenario_compare<-compare_performance(models3,rank=T)



write.csv(scenario_compare,"Outputs/space_time_subset_models_updated.csv",row.names = F)
#Note models were fit from different data...




#Make table of species coefficients in each model
sp_list<-c("TUVU","MODO","HOSP","OSPR","RTHA",
           "RWBL","PIWO","RBWO","NOFL","EUST","AMCR")

df.sp<-df.sp.p<-df.sp.se<-as.data.frame(matrix(ncol = 5 , nrow= length(sp_list)))

colnames(df.sp)<-colnames(df.sp.p)<-colnames(df.sp.se)<-names(models3)


row.names(df.sp)<-row.names(df.sp.p)<-row.names(df.sp.se)<-sp_list


#How do we report coefficient significance...
for (i in length(models3)) {
  
  coefs <- as.data.frame(round(coef(summary(models3[[5]])),3))#need to input models manually
  coefs <- coefs[rownames(coefs)%in%sp_list,]
  df.sp[,5]<-coefs[,1]
  df.sp.p[,5]<-coefs[,3]
  df.sp.se[,5]<-coefs[,2]
  
}
write.csv(df.sp,"Outputs/species_coefficients_updated.csv",row.names = T)


write.csv(df.sp.p,"Outputs/species_coefficients_signif_updated.csv",row.names = T)


write.csv(df.sp.se,"Outputs/species_coefficients_sterror_updated.csv",row.names = T)

