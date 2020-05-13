####HEADER####
rm(list = ls())
getwd()

library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()

insulation <- read_csv("datasets/demos/insulation.csv")