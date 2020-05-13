####HEADER####
rm(list = ls())
getwd()

library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()

#reading data#
insulation <- read_csv("datasets/demos/insulation.csv")

#Predictor Variable: Leanness
#Response variable: Heat loss
#H0: mu1-mu2=0
# I think that an two-sided t.test is the best test to use, so I will see if the data meets the assumptions

ggplot(insulation, aes(x = leanness, y = heat_loss))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(insulation) +
  geom_histogram(aes(leanness), binwidth = 1)+
  facet_wrap(~heat_loss)
ggplot(insulation)+
  geom_qq(aes(sample = heat_loss, color = leanness))

#this data meets the assumptions of a t.test, and no transformation is necessary

t.test(insulation$heat_loss, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#We found that there was a significant difference in heat loss between lean and fatty individuals
#(one-sample, two-sided t.test: t = 4.264, df = 11, p-value < 0.05)


