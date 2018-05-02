# Day 6 with AJ
# April 26th, 2018
# Hafeez Sonday
# Confidence intervals
# chapter 10

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)
library(rcompanion)

# What are confidence intervals? ------------------------------------------

# a range of values so defined that there is a specified 
 # probability that the value of a parameter lies within it.

# Load data ---------------------------------------------------------------

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
head(data)

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# one-way data
# will calculate mean and con inter for males and females respectively.
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
summary(dat1)
# visualise confidence intervals

ggplot(data = dat1, aes(x = Sex, y = Mean)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper, colour = Teacher)) +
  facet_wrap(~Teacher)

# by bootstrapping (takes multiple samples from pile of data)
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

groupwiseMean(Steps ~ Teacher + Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

# Chapter 11, testing assumptions -----------------------------------------


# Load data ---------------------------------------------------------------
# shapiro tests if data is normally distributed

chicks <- as_tibble(ChickWeight)
  shapiro.test(chicks$weight)


# 11.2, transform data ----------------------------------------------------

data1 <- read.table(textConnection(Input),header = TRUE) %>% 
    mutate(ln_steps = log(Steps), #adding columns, everything before = is name and after is actual values
           log10_steps = log10(Steps), 
           sqrt_steps = sqrt(Steps),
           Cubrt = Steps^(1/3)) %>% 
    select(-Student, -Rating, -Steps) %>% # this chucks info out
    gather(key = "data_type", value = "trans_data", 
           -Sex, -Teacher)
head(data1)

ggplot(data = data1, aes(x = trans_data)) +
  geom_histogram(binwidth = 1000, aes(fill = Sex)) +
  facet_grid(data_type ~ Teacher)

# analysis of variance ----------------------------------------------------

iris_dat <- iris 

# H0: there is not a sig diff in petal width between the three iris species.

shapiro.test(iris$Petal.Width)

iris_dat <- as.tibble(iris)
 iris %>% 
   group_by(Species) %>% 
   summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))
# some of the species have non-normal data (virginica)
# do a kruskal-wallis test instead of an ANOVA
 
kruskal.test(Petal.Width ~ Species, data = iris)
