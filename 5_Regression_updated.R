library(tidyverse)
library(ggcorrplot)
library(AICcmodavg)
library(regclass)
library(lme4)
library(performance)


load("Objects and Data/4_dp_outs_towns_updated.rda")
load("Objects and Data/0_species_list.rda")

dp<-dp_outs_towns%>%
  #remove missing data
  filter(saidi!=0 & !(is.na(RWBL)))%>%
  mutate(week=as.factor(week),
         year=as.factor(year),
         month=as.factor(month))

#Don't use interactions
#Do models with all species, representative species ( 1 from each of the 3 groups in PC1 and PC2), PC1 and PC2


#Use R2 conditional (fixed + random; variation explained by random effect, adjusted for each town), 
#
#BIC weight (1 = small delta) as model ranking indices


#xtable r package https://cran.r-project.org/web/packages/xtable/vignettes/xtableGallery.pdf 
#xftbl <- xtableFtable(tbl, method = "compact")
#                          print.xtableFtable(xftbl, booktabs = TRUE)


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




#-------------------------------------------------------------------------------------------------
#Model SAIDI with species DPs, habitat, and time


#1. Determine if species activity explains additional variance than just time and habitat

#Select a species*time*habitat model using backwards selection
##############################################################
#11 species (removing correlated blackbirds and woodpeckers) and their interactions with 12 months + forest habitat

  #Round 1.
m.sth1<-lm(log_saidi~(TUVU*month)+(MODO*month)+(HOSP*month)+(OSPR*month)+(RTHA*month)+(RWBL*month)+
  (PIWO*month)+(RBWO*month)+(NOFL*month)+(EUST*month)+(AMCR*month)+(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
  (RTHA*Forest)+(RWBL*Forest)+
  (PIWO*Forest)+(RBWO*Forest)+
  (NOFL*Forest)+(EUST*Forest)+(AMCR*Forest)+year+
  +Barren_Land+Open_Water+Grassland,data=dp)

m.sth1<-lm(log_saidi~(TUVU*season)+(MODO*season)+(HOSP*season)+(OSPR*season)+(RTHA*season)+(RWBL*season)+
             (PIWO*season)+(RBWO*season)+(NOFL*season)+(EUST*season)+(AMCR*season)+(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
             (RTHA*Forest)+(RWBL*Forest)+
             (PIWO*Forest)+(RBWO*Forest)+
             (NOFL*Forest)+(EUST*Forest)+(AMCR*Forest)+year+
             +Barren_Land+Open_Water+Grassland,data=dp)

summary(m.sth1)
#Significant predictors:
#(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+(RWBL*Forest)+(RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+
#(EUST*month)+(NOFL*month)+(PIWO*month)+(RWBL*month)+(RTHAmonth)+(NOFL*month)+(HOSP*month)+
#Barren_Land+NOFL+RBWO+RWBL+RTHA+MODO+month3

  #Round 2.
m.sth2<-lm(log_saidi~(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+(RWBL*Forest)+
             (RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+(EUST*month)+(NOFL*month)+
             (PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
             Barren_Land,data=dp)


m.sth2<-lm(log_saidi~(MODO*season)+(OSPR*season)+(RTHA*season)+(RWBL*season)+
             (MODO*Forest)+(HOSP*Forest)+
             (RTHA*Forest)+(RWBL*Forest)+
             (PIWO*Forest)+EUST+RBWO+
             (NOFL*Forest)+(EUST*Forest)+year+
             +Barren_Land,data=dp)


summary(m.sth2)
#Significant predictors:
#(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+(RWBL*Forest)+(RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+
#(NOFL*month)+(PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
#Barren_Land+NOFL+RBWO+RWBL+RTHA+MODO+month5+month2+MODO+RTHA+RWBL+NOFL+Forest


#Round 3.
m.sth3<-lm(log_saidi~(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+(RWBL*Forest)+
             (RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+(NOFL*month)+
             (PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
             Barren_Land,data=dp)

m.sth3<-lm(log_saidi~(MODO*season)+(RTHA*season)+(RWBL*season)+
             (MODO*Forest)+(HOSP*Forest)+
             (RTHA*Forest)+(RWBL*Forest)+
             (PIWO*Forest)+EUST+RBWO+
             (NOFL*Forest)+(EUST*Forest)+year+
             +Barren_Land,data=dp)
summary(m.sth3)
#Significant predictors:
#(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+(RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+
#(NOFL*month)+(PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
#Barren_Land+NOFL+RBWO+RWBL+RTHA+MODO+month2+MODO+RTHA+RWBL+NOFL+Forest


#Round 4.
m.sth4<-lm(log_saidi~(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+
             (RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+(NOFL*month)+
             (PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
             Barren_Land,data=dp)

summary(m.sth4)


#Total Species removed: 4

#Use a mixed model to account for random effect of town
m.sth4.lmer<-lmer(log_saidi~(EUST*Forest)+(NOFL*Forest)+(PIWO*Forest)+
             (RTHA*Forest)+(HOSP*Forest)+(MODO*Forest)+(NOFL*month)+
             (PIWO*month)+(RWBL*month)+(RTHA*month)+(NOFL*month)+(HOSP*month)+
             Barren_Land+(1|actual_city_town),data=dp)

m.sth3.lmer<-lmer(log_saidi~(MODO*season)+(RTHA*season)+(RWBL*season)+
             (MODO*Forest)+(HOSP*Forest)+
             (RTHA*Forest)+(RWBL*Forest)+
             (PIWO*Forest)+EUST+RBWO+
             (NOFL*Forest)+(EUST*Forest)+year+
             Barren_Land+(1|actual_city_town),data=dp)

anova(m.sth3.lmer,m.sth3)
#Random effect is significant


#Select a Habitat and Time model using backwards selection
#############################################################
m.th1<-lm(log_saidi~month+year+Forest+Barren_Land+Open_Water+Grassland,data=dp)
m.th1<-lm(log_saidi~season+year+Forest+Barren_Land+Open_Water+Grassland,data=dp)
summary(m.th1)
#Significant predictors: Barren_Land+Forest+month11+month7+month6 (Open_Water and Grassland get dropped just like the species*hab*time model subsets)


m.th2<-lm(log_saidi~month+year+Forest+Barren_Land,data=dp)
m.th2<-lm(log_saidi~season+year+Forest+Barren_Land,data=dp)
summary(m.th2)

#Use a mixed model to account for random effect of town
m.th2.lmer<-lmer(log_saidi~month+year+Forest+Barren_Land+(1|actual_city_town),data=dp)
m.th22.lmer<-lmer(log_saidi~season+year+Forest+Barren_Land+(1|actual_city_town),data=dp)

anova(m.th2.lmer,m.th2)
#random effect is significant



#Compare model performance between models with and without species in a table
models <- list(m.sth3.lmer,m.sth4.lmer,m.th2.lmer,m.th22.lmer)

names(models)<-c('Species.Time(season).Habitat','Species.Time(month).Habitat','Time(month).Habitat','Time(season).Habitat')


var_compare<-compare_performance(models,rank=T)
#add the number of coefficients in each model
Fixed.Effects<-data.frame(Name=names(models),Fixed.Effects=unlist(lapply(models,function(x){length(fixef(x))})))
var_compare<-left_join(var_compare,Fixed.Effects,by="Name")

write.csv(var_compare,"Outputs/predictor_selection_models_monthseason_updated.csv",row.names = F)



#2.Compare models using species from distinct spatial and temporal patterns from the PCA
#RBWO and RTHA represent residents vs OSPR and RWBL who represent summer migrants
#PIWO and TUVU occupy rural areas vs HOSP and NOFL occupying urban areas
m.migrant2<-lmer(log_saidi~(RWBL*season)+OSPR+year+
                   Barren_Land+(1|actual_city_town),data=dp)
m.resident2<-lmer(log_saidi~(RTHA*season)+RBWO+year+
                    Barren_Land+(1|actual_city_town),data=dp)
m.rural2<-lmer(log_saidi~TUVU+(PIWO*Forest)+year+
                 Barren_Land+(1|actual_city_town),data=dp)
m.urban2<-lmer(log_saidi~HOSP+(NOFL*season)+year+
                 Barren_Land+(1|actual_city_town),data=dp)



#Compare model performance in a table
#define list of models
models2 <- list(m.sth2,m.resident2,m.migrant2,m.rural2, m.urban2)

#specify model names
names(models2) <- c('All.Species',
                    'Residents', 
                    'Migrants', 
                    'Rural',
                    'Urban')

pattern_compare<-compare_performance(models2,rank=T)


write.csv(pattern_compare,"Outputs/species_subset_models_updated.csv",row.names = F)

#Multispecies model is the best
#Bird outages seem most correlated with activity levels of spring/fall migrants
#Time over Habitat

sth.res = resid(m.sth2)
hist(sth.res)

#Fit 1 model, 
#make indicator of observations above 75th quantile for habitat
#mkae indicator of observation just within summer and winter
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

