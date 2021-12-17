library(nlme)
library(emmeans)
library(performance)
library(tidyverse)
library(codyn)
library(vegan)

#run this for sum of squares
options(contrasts=c('contr.sum','contr.poly')) #run this first, important to make sure your sum of squares work right

##function for making graphs with summary statistics
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  


#load data
damage <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/damage_2021_combined.csv")
plots <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/damagedemoplots.csv")
treenum <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/treenum.csv")

#merge dataframes
damage1 <- damage %>%
  merge(plots, by = "Plot") %>%
  merge(treenum, by = "Tree_num")

#tried running an ANOVA without repeated measures
summary(insectherbModel <- aov(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class)*as.factor(Sample_period), data=damage1))
anova(insectherbModel)


#this is the model we want but doesn't currently run
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class), data=damage1, random=~1|uniqueplant_num, correlation=corCompSymm(form=~1|uniqueplant_num), control=lmeControl(returnObject = T)))
##this model doesn't work because we included plot age class in the fixed effects and there aren't all interactions in all plot age classes (i.e not all species at each plant age class)

# does herbivory change by plot age and species?
summary(insectherbPlotAgeModel <- glm(Percent_total_herbivory~plot_age*as.factor(Species), data = damage1))
anova(insectherbPlotAgeModel)

#Percent total herbivory
##this works!! use this model
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(insectherbModel, type = 'sequential')
emmeans(insectherbModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Percent_total_herbivory", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))


#Percent_microbial
summary(microbialModel <- lme(Percent_microbial~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(microbialModel, type = 'sequential')
emmeans(microbialModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Percent_microbial", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))


#Percent_microbial
summary(totaldamageModel <- lme(Total_percent_damage~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(microbialModel, type = 'sequential')
emmeans(microbialModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Total_percent_damage", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))






##code from Kim below
#below is the model
summary(insectherbModel <- lme(avg_perc_herbivory~as.factor(species)*as.factor(ind_age)*as.factor(stand_age), #typical model with percent herbivory as a dependent variable and species, individual age, and stand age as explanatory variables
                               data=herbivoryData, #the dataframe the model is being run on
                               random=~1|plot, #random statement for which plot the plant came from
                               correlation=corCompSymm(form=~date|plot/plant), #compound symmetry correlation structure (good for herbivory, which isn't necessarily increasing through time), saying that plants are nested within plots, which were sampled multiple dates
                               control=lmeControl(returnObject=T))) #I forget what this does, but I think it is important to keep

#this checks that the model ran ok (checks assumptions, might not work exactly right on this model though)
check_model(insectherbModel)

#this gives you your p-values
anova.lme(insectherbModel, type='sequential') 


#this gives you the mean and standard errors for each treatment
emmeans(insectherbModel, pairwise~as.factor(stand_age), adjust="tukey")




#same model with a sqrt transformation to make the data normal
summary(insectherbModel <- lme(sqrt(avg_perc_herbivory)~as.factor(species)*as.factor(ind_age)*as.factor(stand_age), #note how avg_perc_herbivory is square root transformed in this one
                               data=herbivoryData, 
                               random=~1|plot, 
                               correlation=corCompSymm(form=~date|plot/plant),
                               control=lmeControl(returnObject=T)))
check_model(insectherbModel)
anova.lme(insectherbModel, type='sequential') 
back.emmeans(emmeans(insectherbModel, pairwise~as.factor(stand_age), adjust="tukey"), transform='sqrt') #back.emmeans will backtransform the values for you 