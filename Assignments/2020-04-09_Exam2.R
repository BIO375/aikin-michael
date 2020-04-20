####Header####
rm(list = ls())
getwd

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

library(tidyverse)
tidyverse_update()

####Question 17####

antibody <- read_csv("datasets/demos/baker.csv") %>%
  mutate(diff = After - Before)

#Response variables = Before and After
#Predictor Variable = Subject

#Ho: mu difference = 0
#Ha: mu difference > 0
#the appropriate statistical test is a one-sided, paired t-test

ggplot(antibody) +
  geom_histogram(aes(diff), binwidth = 1)

ggplot(antibody) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(antibody)+
  geom_qq(aes(sample = diff))

#the data has a two outliers and makes the data unevenly distributed.
#The best solution is to mutate and use the log of the data.

antibody <- read_csv("datasets/demos/baker.csv") %>%
  mutate(log(After)) %>%
  mutate(log(Before)) %>%
  mutate(diff_logs = log(After) - log(Before))

ggplot(antibody) +
  geom_histogram(aes(diff_logs), binwidth = 1)

ggplot(antibody) +
  geom_boxplot(aes(x = "", y = diff_logs))+
  stat_summary(aes(x = "", y = diff_logs), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(antibody)+
  geom_qq(aes(sample = diff_logs))
#the data now meets the assumptions of the test

t.test(antibody$After, antibody$Before, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

#the concentration of antibodies after vaccination was greater than concentration before vaccination
#(one-sided, paired t-test; t = 1.923, df = 19, p = 0.034)

####Question 18####
install.packages("abd", repos="http://R-Forge.R-project.org")
library(abd)

algae <- AlgaeCO2

#response variable: growth rate
#predictor variable: Treatment (high CO2 and normal CO2)

#Ho: muHigh - muNormal = 0
#Ha: muHigh - muNormal not = 0
#The test to run is a two-sample t-test
ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = 2)+
  facet_wrap(~treatment)

ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))+
  stat_summary(aes(x = treatment, y = growthrate), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(algae)+
  geom_qq(aes(sample = growthrate, color = treatment))

#looking at the plots, data is evenly distributed

summ_algae <- algae %>%
  group_by(treatment) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())

ratio <-(max(summ_algae$sd_growthrate))/(min(summ_algae$sd_growthrate))
#the smax/smin ratio is less than 3

t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
#There was no difference in growth rate between algae in high and normal CO2 concentration.
#(two-sample, two-sided t-test; t = -0.53606, df = 12, p > 0.05)


### Code runs without breaking 10/10 ####