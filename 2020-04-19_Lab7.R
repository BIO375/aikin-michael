#### Header #### 
rm(list = ls())
getwd()

install.packages("ggfortify")
library("ggfortify")

install.packages("multcomp")
library("multcomp")

install.packages("nlme")
library("nlme")

library("tidyverse")
tidyverse_update()

#### Question 1 ####
jaffe <- read_csv("datasets/demos/Jaffe.csv")

head(jaffe)
summary(jaffe)

####Aldrin####
#Plots for Aldrin
ggplot(jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 0.1)+
  facet_wrap(~Depth)

ggplot(jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))
#these plots are not that normal

#the ANOVA
model01 <- lm(Aldrin~Depth, data = jaffe)

#checking assumptions
summ_aldrin <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_aldrin = mean(Aldrin),
            sd_aldrin = sd(Aldrin),
            n_aldrin = n())

ratio <-(max(summ_aldrin$sd_aldrin))/(min(summ_aldrin$sd_aldrin))
#the ratio is greater than 3 so transformations must be made

autoplot(model01)

anova(model01)

####Aldrin log####
jaffe <- read_csv("datasets/demos/Jaffe.csv") %>%
  mutate(log10(Aldrin))

#log10 plots
ggplot(jaffe, aes(x = Depth, y = log10(Aldrin)))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(jaffe) +
  geom_histogram(aes(log10(Aldrin)), binwidth = 0.1)+
  facet_wrap(~Depth)

ggplot(jaffe)+
  geom_qq(aes(sample = log10(Aldrin), color = Depth))
#these plots are still not normal, but they are better than they were before.

summ_logaldrin <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_logaldrin = mean(log10(Aldrin)),
            sd_logaldrin = sd(log10(Aldrin)),
            n_logaldrin = n())

ratio <-(max(summ_logaldrin$sd_logaldrin))/(min(summ_logaldrin$sd_logaldrin))
#But the ration is less than 3

model02 <- lm(log10(Aldrin)~Depth, data = jaffe)
autoplot(model02)
anova(model02)

#Tukey HSD 
model01_b <- aov(log10(Aldrin) ~ Depth, jaffe)
TukeyHSD(model01_b)

####HBC####
#Plots for HBC
ggplot(jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()

ggplot(jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.1)+
  facet_wrap(~Depth)

ggplot(jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))
#these plots follow normality

#checking assumptions
summ_aldrin2 <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())

ratio <-(max(summ_aldrin2$sd_HCB))/(min(summ_aldrin2$sd_HCB))
# the ratio is < 3
# the data meets the assumptions

model03 <- lm(HCB~Depth, data = jaffe)
autoplot(model03)
anova(model03)
