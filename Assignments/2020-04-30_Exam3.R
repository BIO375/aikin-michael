####HEADER####
rm(list = ls())
getwd()

library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()

####EXAM 3 CODE####
#reading in caffeine data
caffeine <- read_csv("datasets/demos/caffeine.csv")

#before beginning the study, two alternative hypothesis were identified
#HA1: the mean caffeine metabolism rate is different between men and women without elevated progesterone
#HA2: the mean caffeine metabolism rate is different between women without elevated progesterone and women with
# these comparisons will be alright because the number of them is equal to 2

#checking normaility of the data
ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 1)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

summ_caffeine <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_hl = mean(half_life),
            sd_hl = sd(half_life),
            n_hl = n())

ratio <-(max(summ_caffeine$sd_hl))/(min(summ_caffeine$sd_hl))
#the ratio is less than 3, but it appears that there are two major outliers

#I will run the ANOVA and then the t.tests for the planned comparisons.
modelcaffeine = lm(half_life~group, data = caffeine)
autoplot(modelcaffeine)
anova(modelcaffeine)

#The planned tests
planned_comparisons<- glht(modelcaffeine, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "high_prog - norm_prog = 0")))

#didn't know what was going on with the code, so I just went ahead and did the Tukey's HSD
modelcaffeine2 <- aov(half_life ~ group, caffeine)
TukeyHSD(modelcaffeine2)

#there was a difference between norm_prog and high_prog
#there is no difference between Norm_prog and male

#We found that there is a difference in caffeine half-life among treatments. (one-way ANOVA: df; 2,28, F = 13.86, p < 0.001)

#we found that there is a difference in caffeine half-life between the normal progesterone group and the High progesterone group
#(tukey's HSD: p < 0.05)

#we found that there is no difference in caffeine half-life between the normal progesterone group and the male group
#(tukey's HSD: p > 0.05)
