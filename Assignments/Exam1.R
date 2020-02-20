#Start Up#
rm(list = ls())
getwd()

#install packages, update tidyverse, open tidyverse#
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

tidyverse_update()
library(tidyverse)

####Question 14####
#reading file and creating object#
polyploid <- read_csv("datasets/demos/polyploid.csv")

#summary of data using the descr() function#
polyploid %>%
  group_by(ploidy) %>%
  descr()

#boxplot using facet_wrap to separate 2N and 4N#
ggplot(polyploid)+
  geom_boxplot(aes(x = "ploidy", y = length), notch = FALSE,varwidth = TRUE)+
  facet_wrap(~ploidy)

####Question 15####
#reading file and creating object#
lizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

#summary stats on data for part d."
lizards %>%
  descr()

####Extra Credit####

cricket <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

#creating histograms#

ggplot(cricket) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

#mutating data to include log(timeToMating)#

cricket <- cricket %>%
  mutate(logtimetomating = log(timeToMating))

#creating histogram for mutated data#

ggplot(cricket) +
  geom_histogram(aes(logtimetomating), binwidth = 1)+
  facet_wrap(~feedingStatus)






