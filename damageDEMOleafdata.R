library(nlme)
library(emmeans)
library(performance)
library(tidyverse)
library(codyn)
library(lme4)
library(pscl)
library(ggpubr)

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
damage2 <- damage %>%
  merge(plots, by = "Plot") %>%
  merge(treenum, by = "Tree_num")

#filter for only sapling and mature
damage1 <- damage2 %>%
  filter(Age_class == "Sapling" | Age_class == "Mature")

#tried running an ANOVA without repeated measures
summary(insectherbModel <- aov(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class)*as.factor(Sample_period), data=damage1))
anova(insectherbModel)


#this is the model we want but doesn't currently run
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class), data=damage1, random=~1|uniqueplant_num, correlation=corCompSymm(form=~1|uniqueplant_num), control=lmeControl(returnObject = T)))
##this model doesn't work because we included plot age class in the fixed effects and there aren't all interactions in all plot age classes (i.e not all species at each plant age class in all plot age classes)

##trying again with nesting
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(plot_age_class), data=damage1, random=~1|plot_age_class/Plot/Age_class/Tree_num, correlation=corCompSymm(form=~1|plot_age_class/Plot/Age_class/Tree_num), control=lmeControl(returnObject = T)))


# does herbivory change by plot age and species?
summary(insectherbPlotAgeModel <- glm(Percent_total_herbivory~plot_age*as.factor(Species), data = damage1))
anova(insectherbPlotAgeModel)

###Percent total herbivory###
##this works!! use this model
summary(insectherbModel <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/Age_class/Tree_num), control=lmeControl(returnObject = T)))

anova.lme(insectherbModel, type = 'sequential')
emmeans(insectherbModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

#Species (F=30.578, p<0.001)
#Sample period(F= 15.41, p=0.0001)
#Species*sample period (F=10.43558 p<.0001)
#Species*age class (F=3.164, p=0.0423)


hist(damage1$Percent_total_herbivory)

geom_qq(damage1$Percent_total_herbivory)

plot(insectherbModel)

emmeans(insectherbModel, pairwise~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), adjust="tukey")

###model again with seedling data
summary(insectherbModel2 <- lme(Percent_total_herbivory~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage2, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(insectherbModel2, type = 'sequential')
emmeans(insectherbModel2, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")



damage1$Age_class <- factor(damage1$Age_class, levels = c("Sapling", "Mature"))
damage2$Age_class <- factor(damage2$Age_class, levels = c("Seedling", "Sapling", "Mature"))

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=24, vjust=-0.35), axis.text.x=element_text(size=20, color = "black"),
             axis.title.y=element_text(size=24, angle=90, vjust=0.7), axis.text.y=element_text(size=20, color= "black"),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=18), panel.border=element_rect(color="black", fill = NA, size = 1))

ggplot(data=barGraphStats(data=damage1, variable="Percent_total_herbivory", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4"))+
  ylab('Mean Percent Herbivore Damage\n')+
  xlab(element_blank())+
  expand_limits(y=4) +
  annotate("text", x= 0.77, y = 3.5, label= "a", size = 6)+
  annotate("text", x= 1.23, y = 3.7, label= "a", size = 6)+
  annotate("text", x= 1.77, y = 2.7, label= "b", size = 6)+
  annotate("text", x= 2.23, y = 2.2, label= "b", size = 6)+
  annotate("text", x= 2.77, y = 3.4, label= "ab", size = 6)+
  annotate("text", x= 3.23, y = 3.8, label= "a", size = 6)+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  expand_limits(y=9)

ggplot(data=barGraphStats(data=damage2, variable="Percent_total_herbivory", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4", "darkolivegreen"))+
  ylab('Mean Percent Herbivore Damage\n')+
  xlab(element_blank())+
  expand_limits(y=4) +
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  expand_limits(y=9)

unique(damage2$Species)


ggplot(data=damage1, aes(x=Species, y=Percent_total_herbivory, fill=Age_class))+
  geom_violin()

damageMature <- damage1 %>%
  filter(Age_class == "Mature")

damageSapling<- damage1 %>%
  filter(Age_class == "Sapling")

damageMature$plot_age_class<- as.factor(damageMature$plot_age_class)
damage1$plot_age_class <- as.factor(damage1$plot_age_class)

ggplot(data=barGraphStats(data=damageMature, variable="Percent_total_herbivory", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Herbivory Damage\n')+
  xlab(element_blank())

ggplot(data=barGraphStats(data=damage1, variable="Percent_total_herbivory", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Herbivory Damage\n')+
  xlab(element_blank())

###Percent microbial damage###
summary(microbialModel <- lme(Percent_microbial~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(microbialModel, type = 'sequential')
emmeans(microbialModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

#everything is significant

ggplot(data=barGraphStats(data=damage1, variable="Percent_microbial", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme_bw()+
  scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4"))+
  ylab('Mean Percent Microbial Damage\n')+
  xlab(element_blank())+
  annotate("text", x= 0.77, y = 2.7, label= "c", size = 6)+
  annotate("text", x= 1.23, y = 2.8, label= "c", size = 6)+
  annotate("text", x= 1.77, y = 7.9, label= "b", size = 6)+
  annotate("text", x= 2.23, y = 9.3, label= "a", size = 6)+
  annotate("text", x= 2.77, y = 9.5, label= "ab", size = 6)+
  annotate("text", x= 3.23, y = 8.7, label= "b", size = 6)+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  expand_limits(y=9)

ggplot(data=barGraphStats(data=damage2, variable="Percent_microbial", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme_bw()+
  scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen4", "darkolivegreen"))+
  ylab('Mean Percent Microbial Damage\n')+
  xlab(element_blank())+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  expand_limits(y=9)


ggplot(data=damage1, aes(x=Species, y=Percent_microbial, fill=Age_class))+
  geom_boxplot()

ggplot(data=barGraphStats(data=damageMature, variable="Percent_microbial", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Microbial Damage\n')+
  xlab(element_blank())
  
ggplot(data=barGraphStats(data=damage1, variable="Percent_microbial", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Microbial Damage\n')+
  xlab(element_blank())
  
#Total percent damage
summary(totaldamageModel <- lme(Total_percent_damage~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(totaldamageModel, type = 'sequential')
emmeans(totaldamageModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Total_percent_damage", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("grey40", "grey"))+
  ylab('Mean Percent Damage\n')+
  xlab(element_blank())+
  expand_limits(y=14)+
  annotate("text", x= 0.77, y = 6.5, label= "d", size = 4)+
  annotate("text", x= 1.23, y = 6.3, label= "d", size = 4)+
  annotate("text", x= 1.77, y = 11.6, label= "b", size = 4)+
  annotate("text", x= 2.23, y = 10.9, label= "c", size = 4)+
  annotate("text", x= 2.77, y = 12.3, label= "a", size = 4)+
  annotate("text", x= 3.23, y = 12.6, label= "ab", size = 4)

ggplot(data=barGraphStats(data=damageMature, variable="Total_percent_damage", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Damage\n')+
  xlab(element_blank())


##chewing model
summary(chewingModel <- lme(Percent_chewed~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(chewingModel, type = 'sequential')
emmeans(chewingModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Percent_chewed", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("grey40", "grey"))+
  ylab('Mean Percent Chewed\n')+
  xlab(element_blank())+
  expand_limits(y=5)

ggplot(data=damage1, aes(x=Species, y=Percent_chewed, fill=Age_class))+
  geom_boxplot()

ggplot(data=barGraphStats(data=damageMature, variable="Percent_chewed", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Chewed\n')+
  xlab(element_blank())

######skeletonizing
summary(skeletonizedModel <- lme(Percent_skeletonized~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(skeletonizedModel, type = 'sequential')
emmeans(skeletonizedModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Percent_skeletonized", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("grey40", "grey"))+
  ylab('Mean Percent Skeletonized\n')+
  xlab(element_blank())+
  expand_limits(y=1)

ggplot(data=damage1, aes(x=Species, y=Percent_skeletonized, fill=Age_class))+
  geom_boxplot()

ggplot(data=barGraphStats(data=damageMature, variable="Percent_skeletonized", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Skeletonized\n')+
  xlab(element_blank())

#####mining
summary(miningModel <- lme(Total_percent_damage~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(miningModel, type = 'sequential')
emmeans(miningModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")

ggplot(data=barGraphStats(data=damage1, variable="Percent_mined", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("grey40", "grey"))+
  ylab('Mean Percent Mining\n')+
  xlab(element_blank())+
  expand_limits(y=1)

ggplot(data=damage1, aes(x=Species, y=Percent_mined, fill=Age_class))+
  geom_boxplot()

ggplot(data=barGraphStats(data=damageMature, variable="Percent_mined", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Mining\n')+
  xlab(element_blank())


#####percent gall
summary(percentgallModel <- lme(Total_percent_damage~as.factor(Species)*as.factor(Age_class)*as.factor(Sample_period), data=damage1, random=~1|plot_age_class/Plot, correlation=corCompSymm(form=~1|plot_age_class/Plot/plant_num), control=lmeControl(returnObject = T)))

anova.lme(percentgallModel, type = 'sequential')
emmeans(percentgallModel, pairwise~as.factor(Species)*as.factor(Age_class), adjust="tukey")



ggplot(data=barGraphStats(data=damage1, variable="Percent_gall", byFactorNames=c("Species", "Age_class")), aes(x=Species, y=mean, fill=Age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  scale_fill_manual(values=c("grey40", "grey"))+
  ylab('Mean Percent Gall\n')+
  xlab(element_blank())+
  expand_limits(y=1.5)

ggplot(data=damage1, aes(x=Species, y=Percent_gall, fill=Age_class))+
  geom_boxplot()


ggplot(data=barGraphStats(data=damageMature, variable="Percent_gall", byFactorNames=c("Species", "plot_age_class")), aes(x=Species, y=mean, fill=plot_age_class)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+
  theme(legend.title=element_blank())+
  theme_bw()+
  ylab('Mean Percent Gall\n')+
  xlab(element_blank())


####Regressions
##Total herbivory
herbivory_lm <- lm(Percent_total_herbivory ~ plot_age*Species*Age_class*Sample_period, data = damage1)
summary(herbivory_lm)
#Age_class, sampling period

predictedherbivory <- predict(herbivory_lm, interval = "confidence", level = 0.95) 

full_herbivory <- data.frame(damage1, predictedherbivory)


ggplot(full_herbivory, aes(x = plot_age, y = Percent_total_herbivory))+
  geom_point(aes(color= Species))+
  geom_line(aes(x = plot_age, y = fit)) +
  geom_ribbon(aes(x = plot_age, ymin = lwr, ymax = upr), alpha = 0.3) +
  #annotate("text", x = 14, y = 290, label = "y = -7.35x + 161.33")+
  #annotate("text", x = 14, y = 270, label = "italic(R) ^ 2 == 0.14", parse = TRUE)+
  ylab('Percent herbivory')+
  xlab('Plot age')
 
ggplot(subset(damage1, Sample_period == 2 & Age_class == "Mature" & plot_age < 200), aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("#D39200",  "#93AA00", "#00B9E3")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  



number_ticks <- function(n) {function(limits) pretty(limits, n)}
sp.labs <- c("Jul", "Oct")
names(sp.labs) <- c("1", "2")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)

ggplot(damage1, aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  scale_x_continuous(breaks=number_ticks(4)) +
  facet_grid(rows = vars(Age_class), cols = vars(Sample_period), labeller = labeller(Sample_period = sp.labs))+
  theme(strip.text.x = element_text(size = 14, color = "black"), strip.text.y = element_text(size = 14, color = "black"))+
  ylab("log10(Percent Herbivore Damage)")+
  xlab("Plot Age")+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())

ggplot(damage1, aes(x=plot_age, y=log10(Percent_microbial))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  scale_x_continuous(breaks=number_ticks(4)) +
  facet_grid(rows = vars(Age_class), cols = vars(Sample_period), labeller = labeller(Sample_period = sp.labs))+
  theme(strip.text.x = element_text(size = 14, color = "black"), strip.text.y = element_text(size = 14, color = "black"))+
  ylab("log10(Percent Microbial Damage)")+
  xlab("Plot Age")+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())



#mature, sample period 1
ggplot(subset(damage1, Sample_period == 1 & Age_class == "Mature"), aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=0.5, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#mature sample period 2
ggplot(subset(damage1, Sample_period == 2 & Age_class == "Mature"), aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=0.5, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#sapling sample period 1
ggplot(subset(damage1, Sample_period == 1 & Age_class == "Sapling"), aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#sapling sample period 2
ggplot(subset(damage1, Sample_period == 2 & Age_class == "Sapling"), aes(x=plot_age, y=log10(Percent_total_herbivory))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("yellow3",  "chartreuse4", "darkorchid3")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

##Microbial
microbial_lm <- lm(Percent_microbial ~ plot_age, data = damage1)
summary(microbial_lm)

ggplot(subset(damage1, Sample_period == 2 & Age_class == "Mature"), aes(x=plot_age, y=log10(Percent_microbial))) +
  geom_point(aes(color=Species)) + 
  scale_color_manual(values = c("red",  "blue", "green")) +
  geom_smooth(aes(color=Species, fill=Species), size=1, method=lm) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#####stacked bar charts
herbivorywide <- damage1 %>%
  select (Plot, Species, Age_class, Leaf_num, Sample_period, Percent_chewed, Percent_mined, Percent_skeletonized, Percent_gall, plot_age, plot_age_class)

herbivorylong <- gather(herbivorywide, Herbivory_type, Percent_damage, Percent_chewed:Percent_gall, factor_key=TRUE)
herbivorylong

herbivorylong$Sample_period <- as.factor(herbivorylong$Sample_period)

ggplot(data=barGraphStats(data=herbivorylong, variable="Percent_damage", byFactorNames=c("Species", "Sample_period", "Herbivory_type")), aes(x=Sample_period, y=mean, fill=Herbivory_type))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Species)+
  ylab('Mean Percent Damage')+
  xlab('Sample Period')+
  theme(legend.title=element_blank())+
  #facet_grid(rows = vars(Age_class), cols = vars(Species))

herbivorylongMature <- herbivorylong %>%
  filter(Age_class == "Mature")

ggplot(data=barGraphStats(data=herbivorylongMature, variable="Percent_damage", byFactorNames=c("Species", "Sample_period", "Herbivory_type")), aes(x=Sample_period, y=mean, fill=Herbivory_type))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_discrete(labels = c("Chewing", "Mining", "Skeletonizing", "Galls"))+
  facet_wrap(~Species)+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  ylab('Mean Percent Herivore Damage\n')+
  xlab('Sample Period')+
  theme(legend.title=element_blank())+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_discrete(labels = c("Jul", "Oct"))

herbivorylongSapling <- herbivorylong %>%
  filter(Age_class == "Sapling")

ggplot(data=barGraphStats(data=herbivorylongSapling, variable="Percent_damage", byFactorNames=c("Species", "Sample_period", "Herbivory_type")), aes(x=Sample_period, y=mean, fill=Herbivory_type))+
  geom_bar(position="stack", stat="identity", width = 1)+
  scale_fill_discrete(labels = c("Chewing", "Mining", "Skeletonizing", "Galls"))+
  facet_wrap(~Species)+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  ylab('Mean Percent Herbivore Damage\n')+
  xlab('Sample Period')+
  theme(legend.title=element_blank())+
  theme(legend.title=element_blank())+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_discrete(labels = c("Jul", "Oct"))

damage1$Sample_period <- as.factor(damage1$Sample_period)

ggplot(data=barGraphStats(data=damage1, variable = "Percent_microbial", byFactorNames=c("Species", "Sample_period")), aes(x=Sample_period, y=mean))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Species)+
  ylab('Mean Percent Microbial Damage')+
  xlab('Sample Period')


ggplot(data=barGraphStats(data=damageMature, variable = "Percent_microbial", byFactorNames=c("Species", "Sample_period")), aes(x=Sample_period, y=mean))+
  geom_bar(position="dodge", stat="identity",  fill="chocolate2")+
  facet_wrap(~Species)+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  ylab('Mean Percent Microbial Damage\n')+
  xlab('Sample Period')+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_discrete(labels = c("Jul", "Oct"))+
  expand_limits(y=15)

ggplot(data=barGraphStats(data=damageSapling, variable = "Percent_microbial", byFactorNames=c("Species", "Sample_period")), aes(x=Sample_period, y=mean))+
  geom_bar(position="dodge", stat="identity", fill="chocolate2")+
  facet_wrap(~Species)+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  ylab('Mean Percent Microbial Damage\n')+
  xlab('Sample Period')+
  theme(legend.title=element_blank())+
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text.x=element_text(size=16, color = "black"), axis.title.y=element_text(size=18), axis.text.y=element_text(size=16, color= "black"), axis.title.x=element_text(size=18), panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_x_discrete(labels = c("Jul", "Oct"))+
  expand_limits(y=15)






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