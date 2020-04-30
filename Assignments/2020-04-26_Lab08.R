#### Header #### 
rm(list = ls())
getwd()


library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()

####Problem 15-23 a and c####
pines <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv")

#the Comparison in this question is a planned comparison because it was decided before the
#experiment began. 

#checking to see if the data meets the assumptions of the ANOVA
ggplot(pines, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(pines) +
  geom_histogram(aes(conemass), binwidth = 0.1)+
  facet_wrap(~habitat)
ggplot(pines)+
  geom_qq(aes(sample = conemass, color = habitat))

summ_conemass <- pines %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())

ratio <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))
# the data is normal (Thank god)

#making the model for the ANOVA
model01 <- lm(conemass~habitat, data = pines)

autoplot(model01)

anova(model01)

#We found that there was a significant difference between the conemass of pine trees
#on a squirrel absent island and a squirrel present island (one-way anova: df = 2,13, F = 50.1, p<0.001)

####Problem 15-26####
malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv")

#researchers are testing to see if the amount of malaria cells in mosquitoes differs when 
#they are exposed to the Metarhizum anisopliae fungus and the transgenic strain of that same fungus

# checking to see if the data will meet the assumptions of a one-way anova, along with normality of the data
ggplot(malaria, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.1)+
  facet_wrap(~treatmentGroup)
ggplot(malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

summ_malaria <- malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_sporozoites = mean(logSporozoiteNumbers),
            sd_sporozoites = sd(logSporozoiteNumbers),
            n_conemass = n())

ratio <-(max(summ_malaria$sd_sporozoites))/(min(summ_malaria$sd_sporozoites))

#The graphs do not look normal, but the ration is less than three, so I will run a kruskal wallace test

kruskal.test(logSporozoiteNumbers~treatmentGroup, data = malaria)

#we found that there was a significant difference between mosquitoes with the modified fungus
#and mosquitoes with the normal fungus.
#Kruskal-wallis chi squared = 22.874, df = 2, p < 0.0001

####Problem 15-30####
crabs <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv")

crabs <- crabs %>%
  slice(-85)

#We need to see if the data is normal and meets the assumptions
ggplot(crabs, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)
ggplot(crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

summ_crabs <- crabs %>%
  group_by(crabType) %>% 
  summarise(mean_bodytemp = mean(bodyTemperature),
            sd_bodytemp = sd(bodyTemperature),
            n_bodytemp = n())

ratio <-(max(summ_crabs$sd_bodytemp))/(min(summ_crabs$sd_bodytemp))

#the ratio is less than 3, but the data does not meet all of the assumptions 
#with that I will use a welch's ANOVA? because of the variance of each group

oneway.test(bodyTemperature ~ crabType, data = crabs)
