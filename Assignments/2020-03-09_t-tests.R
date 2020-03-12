####Header####
rm(list = ls())
getwd()

tidyverse_update
library(tidyverse)

####Scenario 1####
births <- read_csv("datasets/demos/2020-03-08_birthrates.csv")

#I have some problems with the mutate argument#
#It doesn't alter the data, it just creates a new object#
#that is why i had to name it something else#
births <- births %>%
  mutate(diff = Birth_2000 - Birth_1982)

#even after I renamed the object I couldn't get the stat summary to work#
#there was a typo#
summ_diff <- births %>%
  summarise(meand = mean(diff),
            sdd = sd(diff),
            se_d = sd(diff)/sqrt(n()),
            nd = n())

#calculating t from stat#
t <- summ_diff$meand/summ_diff$se_d

ggplot(data = births)+
  geom_histogram(mapping = aes(x="", y=diff), binwidth = 1)

ggplot(data = births)+
  geom_boxplot(mapping = aes(x="", y=diff),
               fun.y = mean,
               colour = "darkred",
               geom = "point",
               shape = 18,
               size = 3)

ggplot(births)+
  geom_qq(mapping = aes(sample = diff))

#Calculating t statistic
(2.67 - 0)/sqrt(2.97*(1/15 + 1/15))

####Scenario 2####

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
#slice subtracts data row 105#
data01 <- data01 %>% 
  slice(-105)

#don't forget to group by
#don't forget the pipe %>%
summ_horn <- data01 %>%
  group_by(Survival) %>%
  summarise(mean_y = mean(squamosalHornLength),
            sd_y = sd(squamosalHornLength),
            se_y = sd_y/sqrt(n()),
            var_y = var(squamosalHornLength),
            n_y= n())

