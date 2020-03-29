####Header####

rm(list = ls())
getwd()

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

tidyverse_update()

library(tidyverse)

####Question 1####

paris <- read_csv("datasets/demos/2020-03-12_Paris-observatory.csv")

#two-sided because of a certain value
t.test(paris$obliquity, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#We found that the obliquity measured in Paris in 1738 was different when compared with earlier measurements
#One-sample t-test, t=2679.1, df=4, p=1.165e-13, 95%CI = 23.47-23.52, mean x = 23.49

####Question 2####
heart <- read_csv("datasets/demos/HeartAttack_short.csv", 
                  col_types = cols(group = col_character()))

summary_heart <- heart %>%
  group_by(group) %>%
  summarise(meanc = mean(cholest),
            sdc = sd(cholest),
            nc = n(),
            sec = sd(cholest)/sqrt(n()))

#Two-sample Two-tailed
t.test(cholest ~ group, data = heart, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#we found that blood cholesterol in heart-attack patients 2 days post heart attack was
#not equal to the blood cholesterol in individuals who have not had a heart attack
#two-sample Two-tailed; t=6.2852, df = 56, p=5.202e-08, 95%CI = 41.41849-80.17199


####Question 3####
#have to use wilcoxon rank sum test :(

furness <- read_csv("datasets/quinn/chpt3/furness.csv")

ggplot(furness) +
  geom_boxplot(aes(x = METRATE, y = SEX)) +
    stat_summary(aes(x = METRATE, y = SEX), 
                 fun=mean, 
                 colour="blue", 
                 fill = "blue",
                 geom="point", 
                 shape=21, 
                 size=3)
  
ggplot(furness)+
  geom_qq(aes(sample = METRATE, color = SEX))

#data is skewed. female is left skewed, and males are right skewed
#when normality is not met we can use a non-parametric test, or mutate the data (log)

wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

#we found that body mass of male fulmars was not significantly different from body mass of female fulmars
#wilcoxon rank sum test; w=21; p=0.75

####Question 4####
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")

#I need to run a paired, two-sided t-test

t.test(elgar$HORIZDIM, elgar$HORIZLIG,
       alternative = "two.sided", paired = TRUE, mu = 0, conf.level = 0.95)

#We found that horizontal diameter of an orb-spinning spider's web in light conditions was not equal to 
#the horizontal diameter in dark conditions
#two-sided, paired; t=2.1482; df=16; p=0.04735







