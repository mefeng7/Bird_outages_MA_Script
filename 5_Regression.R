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

#1. Determine if species, habitat, or time variables are more important

#species (removing correlated blackbirds and woodpeckers)
m.s<-(lm(log_saidi~TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+NOFL+EUST+AMCR,data=dp))
m.s2<-(lmer(log_saidi~TUVU+MODO+HOSP+OSPR+RTHA+RWBL+RBWO+PIWO+NOFL+EUST+AMCR+(1|actual_city_town),#*****UPDATE 11 SPECIES
         data = dp))

anova(m.s2,m.s)#goodness of fit shows random effect performs better

#Time (week does best but month is close. Season does poorly.)
m.t<-lm(log_saidi~month+year,data=dp)
m.t2<-lmer(log_saidi~month+year+(1|actual_city_town),data=dp)

anova(m.t2,m.t)

#Habitat (Remove forest or developed, correlated. Forest performs better)
m.h<-lm(log_saidi~Forest+Barren_Land+Open_Water+Grassland,data=dp)
m.h2<-lmer(log_saidi~Forest+Barren_Land+Open_Water+Grassland+(1|actual_city_town),data=dp)

anova(m.h2,m.h)

#species+time
m.st<-lm(log_saidi~(TUVU*month)+(MODO*month)+(HOSP*month)+(OSPR*month)+(RTHA*month)+(RWBL*month)+
  (PIWO*month)+(RBWO*month)+(NOFL*month)+(EUST*month)+(AMCR*month)+year,data=dp)
m.st2<-lmer(log_saidi~(TUVU*month)+(MODO*month)+(HOSP*month)+(OSPR*month)+(RTHA*month)+(RWBL*month)+
           (PIWO*month)+(RBWO*month)+(NOFL*month)+(EUST*month)+(AMCR*month)+year+(1|actual_city_town),data=dp)

anova(m.st2, m.st)

#species+habitat 
m.sh<-lm(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
           (RTHA*Forest)+(RWBL*Forest)+
           (PIWO*Forest)+(RBWO*Forest)+
           (NOFL*Forest)+(EUST*Forest)+(AMCR*Forest)+
           Barren_Land,data=dp)
m.sh2<-lmer(log_saidi~(TUVU*Forest)+(MODO*Forest)+(HOSP*Forest)+(OSPR*Forest)+
              (RTHA*Forest)+(RWBL*Forest)+
              (PIWO*Forest)+(RBWO*Forest)+
              (NOFL*Forest)+(EUST*Forest)+(AMCR*Forest)+
              Barren_Land+(1|actual_city_town),data=dp)
  #model using species* forest habitat interaction performs the best 
  # (Highest R2, lowest RSE)

anova(m.sh2, m.sh)

#species+habitat+time
#PIWO has significant habitat interaction
#EUST,RWBL,RTHA,HOSP have significant month interactions
m.sth2<-lmer(log_saidi~TUVU+MODO+(HOSP*month)+OSPR+(RTHA*month)+
            (RWBL*month)+(PIWO*Forest)+RBWO+NOFL+(EUST*month)+AMCR+year+
            Barren_Land+(1|actual_city_town),data=dp)

#time+habitat 
m.th<-lm(log_saidi~month+year+Forest+Barren_Land+Open_Water+Grassland,data=dp)
m.th2<-lmer(log_saidi~month+year+Forest+Barren_Land+Open_Water+Grassland+(1|actual_city_town),data=dp)

anova(m.th2,m.th)



#Compare model performance in a table
models <- list(m.s2, m.t2, m.h2, m.st2, m.sh2, m.sth2,
               m.th2)

names(models)<-c('Species','Time','Habitat','Species.Time',
                 'Species.Habitat', 'Species.Time.Habitat', 
                 'Time.Habitat')
  

var_compare<-compare_performance(models,rank=T)

write.csv(var_compare,"Outputs/predictor_selection_models_updated.csv",row.names = F)



#2.Compare models using species from distinct spatial and temporal patterns from the PCA
#RBWO and RTHA represent residents vs OSPR and RWBL who represent summer migrants
#PIWO and TUVU occupy rural areas vs HOSP and NOFL occupying urban areas
m.migrant2<-lmer(log_saidi~(RWBL*month)+OSPR+year+
                 Barren_Land+(1|actual_city_town),data=dp)
m.resident2<-lmer(log_saidi~(RTHA*month)+RBWO+year+
                Barren_Land+(1|actual_city_town),data=dp)
m.rural2<-lmer(log_saidi~TUVU+(PIWO*Forest)+year+
              Barren_Land+(1|actual_city_town),data=dp)
m.urban2<-lmer(log_saidi~(HOSP*month)+NOFL+year+
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


#3. Compare species*habitat*time models using subsets of data in each season (remove month)
#and in forest vs developed+barren habitat types (remove habitat)
m.summer2<-lmer(log_saidi~TUVU+MODO+(HOSP*month)+OSPR+(RTHA*month)+
                  (RWBL*month)+(PIWO*Forest)+RBWO+NOFL+(EUST*month)+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(season=="summer"))
m.winter2<-lmer(log_saidi~TUVU+MODO+(HOSP*month)+OSPR+(RTHA*month)+
                  (RWBL*month)+(PIWO*Forest)+RBWO+NOFL+(EUST*month)+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(season=="winter"))
m.Forest2<-lmer(log_saidi~TUVU+MODO+(HOSP*month)+OSPR+(RTHA*month)+
                  (RWBL*month)+(PIWO*Forest)+RBWO+NOFL+(EUST*month)+AMCR+year+
                  Barren_Land+(1|actual_city_town),data=dp%>%filter(Forest>quantile(Forest,0.75)))
m.Developed2<-lmer(log_saidi~TUVU+MODO+(HOSP*month)+OSPR+(RTHA*month)+
                     (RWBL*month)+(PIWO*Forest)+RBWO+NOFL+(EUST*month)+AMCR+year+
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

df.sp<-df.sp.p<-as.data.frame(matrix(ncol = 5 , nrow= length(sp_list)))

colnames(df.sp)<-colnames(df.sp.p)<-names(models3)


row.names(df.sp)<-row.names(df.sp.p)<-sp_list


#How do we report coefficient significance...
for (i in length(models3)) {

  coefs <- round(models3[[5]][["coefficients"]],3)#need to input models manually
  coefs <- coefs[names(coefs)%in%sp_list]
  df.sp[[5]]<-coefs
  
}
write.csv(df.sp,"Outputs/species_coefficients_updated.csv",row.names = T)

for (i in length(models)) {

    p<-round(summary(models[[1]])$coefficients[,4],3)
    p <- p[names(p)%in%sp_list]
    df.sp.p[[1]]<-p
  
}
write.csv(df.sp.p,"Outputs/species_coefficients_signif_updated.csv",row.names = T)

