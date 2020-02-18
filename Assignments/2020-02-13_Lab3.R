#write code that will create a new data table in excel
#due Monday february 17

#### Start-up ####

rm(list = ls())
getwd()

tidyverse_update
library(tidyverse)

#### Creating object from Lovett ####

lovett <- read_csv("datasets/quinn/chpt2/lovett.csv")

#Read file first then summarise lovett data#
summ_lovett <- lovett%>%
  summarise(mean_SO4 = mean(SO4),
            median_SO4 = median(SO4),
            IQR_SO4 = IQR(SO4),
            sd_SO4 = sd(SO4),
            se_SO4 = sd(SO4)/sqrt(n()),
            var_SO4 = var(SO4))

#Boxplot for SO4MOD with a notch#
#range is set to 55,70#
#graphs do not have a group so no need for X value#
ggplot(lovett)+
  geom_boxplot(aes(x = "", y = SO4MOD), notch = TRUE,varwidth = TRUE)+
  coord_cartesian(ylim = c(55,70))

ggplot(lovett)+
  geom_boxplot(aes(x = "", y = SO4), notch = TRUE,varwidth = TRUE)+
  coord_cartesian(ylim = c(55,70))

ggplot(lovett)+
  geom_histogram(aes(SO4MOD), binwidth =1)+
  coord_cartesian(xlim = c(50,75))

ggplot(lovett)+
  geom_histogram(aes(SO4), binwidth =1)

#### Creating Object from Sanchez ####

sanchez <- read_csv("datasets/demos/sanchez1.csv")

#read file and sumarise the data#
summ_sanchez <- sanchez %>%
  group_by(BIRDCOLONY) %>%
  summarise(mean_BEETLE96 = mean(BEETLE96),
            median_BEETLE96 = median(BEETLE96),
            IQR_BEETLE96 = IQR(BEETLE96),
            sd_BEETLE96 = sd(BEETLE96),
            se_BEETLE96 = sd(BEETLE96)/sqrt(n()),
            var_BEETLE96 = var(BEETLE96))

#histogram of data before mutation
ggplot(sanchez)+
  geom_histogram(aes(BEETLE96), binwidth = 4)+
  facet_wrap(~BIRDCOLONY)+
  coord_cartesian(xlim = c(0,65))
  

#boxplots of data before mutation for each colony type#
ggplot(sanchez)+
  geom_boxplot(aes(x ="R", y = BEETLE96), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,35))

ggplot(sanchez)+
  geom_boxplot(aes(x ="N", y = BEETLE96), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,35))

ggplot(sanchez)+
  geom_boxplot(aes(x ="B", y = BEETLE96), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,35))

#Mutate the data set to include log(y+1)to get beetle density#
sanchez <- mutate(sanchez, beetledensity = log(BEETLE96+1))

#Mutated data histograms for each colony type#
ggplot(sanchez)+
  geom_histogram(aes(beetledensity), binwidth =0.5)+
  facet_wrap(~BIRDCOLONY)

#Mutated data boxplots for each colony type#
ggplot(sanchez)+
  geom_boxplot(aes(x = "R", y = beetledensity), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,4))
  
ggplot(sanchez)+
  geom_boxplot(aes(x = "B", y = beetledensity), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,4))

ggplot(sanchez)+
  geom_boxplot(aes(x = "N", y = beetledensity), notch = FALSE, varwidth = TRUE)+
  coord_cartesian(ylim = c(0,4))

### Advice ####
# Switch the order of lines 9 and 10.  You need to load the package using
# library() before you can update it.

### GRADE: 10/10 code runs without breaking ####


