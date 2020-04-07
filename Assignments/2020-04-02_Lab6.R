####Header####
rm(list = ls())
getwd()

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

library(tidyverse)
tidyverse_update()

####13-20####
salmon <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

ggplot(salmon) +
  geom_boxplot(aes(x = species, y = skinColor)) +
  stat_summary(aes(x = species, y = skinColor), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(salmon)+
  geom_qq(aes(sample = skinColor, color = species))
#part a.
#The two methods that are appropriate for testing whether there was a difference
#in mean skin color are transforming the data and using a non-parametric test

#part b.
summary(log10(salmon$skinColor))
#after using a transformation to see if there was a difference in mean skin color
#I found that there was no difference in mean.

#The data appears to be normal so
#I think that a two-sample one-sided t-test is the best test for this data
#Ha: kokanee carotenoid concentration > sockeye carotenoid concentration
#Ho: kokanee carotenoid concentration < or = sockeye carotenoid concentration

t.test(skinColor ~ species, data = salmon, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

#Kokanee carotenoid concentration is greater than sockeye carotenoid concentration
#two-sample, one-sided: t = 10.297, df = 33, p < 0.05

####13-25####
cuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

#test will be two-sided. the question asked if there was a change
# it did not specify if they were looking for a negative or positive change
#Ha: mu is not equal to zero
#Ho: mu = 0

ggplot(cuts) +
  geom_boxplot(aes(x = biomassChange, y = "")) +
  stat_summary(aes(x = biomassChange, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(cuts)+
  geom_qq(aes(sample = biomassChange))

#The data is skewed to the left, but I'm not sure what to do about it
#I will run a one-sample, two-sided t-test

t.test(cuts$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#There is no change in biomass of rainforest areas after clear-cutting
#one-sample, two-sided, t = -0.85, df = 35, p = 0.399

####13-26####
beaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

ggplot(beaks) +
  geom_boxplot(aes(x = preference, y = "")) +
  stat_summary(aes(x = preference, y = ""), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(beaks)+
  geom_qq(aes(sample = preference))

#the data is slightly skewed, but I don't think that it will be a problem
#the test I will use is a one-sample, but I am not sure if that is the best option
#the questions asked makes it sound like the test I should be using is a paired test
#however with the data given I can only run a one sample test without knowing which pair of brothers is which.

#Ha: Female preferance of carotenoid supplement males > female preferance of control males
#Ho: Female preferance of carotenoid supplement males </= female preferance of control males

t.test(beaks$preference, 
       alternative = "greater", mu = 0, conf.level = 0.95)

#one-sample, one-sided; t = 5.6198, df = 9, p < 0.05

####Review Problems 2-16####
zebrafish <- read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")

summ_zebra <- zebrafish %>%
  group_by(genotype) %>%
  summarise(means = mean(secondsAggressiveActivity),
            sds = sd(secondsAggressiveActivity),
            ns = n(),
            ses = sd(secondsAggressiveActivity)/sqrt(n()))

#part a.
#the difference between means is 68.1

#part b.
#the correct statistical test would be a two-sample, one-sided t-test
#Ha: mutant agressive time > wild type agressive time
#Ho: mutant agressive time </= wild type agressive time

t.test(secondsAggressiveActivity ~ genotype, data = zebrafish, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

#We found that the mutant genotype was more agressive than the wild type
#two-sample, One-sided: t = 3.3802, df = 19, p < 0.05

