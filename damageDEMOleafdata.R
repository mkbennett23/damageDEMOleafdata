library(nlme)
library(emmeans)
library(performance)
library(tidyverse)

options(contrasts=c('contr.sum','contr.poly')) #run this first, important to make sure your sum of squares work right

#load data
damage <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/damage_2021_combined.csv")
plots <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/damagedemoplots.csv")
treenum <- read.csv("~/Dropbox (Smithsonian)/SERC_damageDemo/treenum.csv")

#merge dataframes
damage1 <- damage %>%
  merge(plots, by = "Plot") %>%
  merge(treenum, by = "Tree_num")

#not sure if this is important/needed
damage1$plot <- as.factor(damage1$plot)
damage1$X <- as.factor(damage1$X)
damage1$Sample_period <- as.factor(damage1$Sample_period)

#tried running an ANOVA without repeated measures
summary(insectherbModel <- aov(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class)*as.factor(Sample_period), data=damage1))

anova(insectherbModel)

#this is the model we want but doesn't currently run
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class), data=damage1, random=~1|X, correlation=corCompSymm(form=~1|X), control=lmeControl(returnObject = T)))



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