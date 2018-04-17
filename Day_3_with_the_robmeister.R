# Hafeez Sonday
# generating Cullen & Frey graph
# Day 3_R with prof smit on the various forms of distributions that
# data may appear to occupy
# April, 17th 2018
# Using a cullen & frey graph may help you visualise where your data
# falls and subsequently what distribution analysis you need to apply

# setup -------------------------------------------------------------------

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(plotly)

# Generating normal data --------------------------------------------------

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

# visualising our normal dataframe, using a histogram ---------------------
hist(r_norm)

#descriptive distribution function
descdist(r_norm, discrete = FALSE, boot = 100)

# uniform data
y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

# T-tests -----------------------------------------------------------------

# Randomised normal data

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Checking assumptions ----------------------------------------------------

# testing Normality
# we use shapiro-wilk test, testing null hypothesis to see 
# wherethe or not data is sig different, which is p value
shapiro.test(r_dat$dat)

# the above test all data together
# however we need to group to test selective data

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) # 2 saummarise all data into 2 values, we only want the 2nd
# remember, the data are normal when p > 0.05
# the data are non-normal when p<= 0.05

# Check homoscedaticily ---------------------------------------------------
# many ways to check for homoscedasticily
# which is the similarity of variance between sample sets
# for now, we meet this assumption
# variance of samples are not greater than 2 - 4 times

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

# One-sample t-test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
# testing normality
shapiro.test(r_dat$dat)

# Visualisation
hist(r_one$dat)

#running the actual test
t.test(r_one$dat, mu = 20) # mu is population mean

# run a test we know will produce a sig result
t.test(r_one$dat, mu = 30)

# picking side with alternative addition to function

# are these data smaller/less than the pop size
t.test(r_one$dat, mu = 20, alternative = "less") #alt test for if these birds are less/greater
# or greater
t.test(r_one$dat, mu = 20, alternative = "greater")

# but what about for the larger pop mean?
# are the samples less than the pop of 30?

t.test(r_one$dat, mu = 30, alternative = "less")
# or greater than
t.test(r_one$dat, mu = 30, alternative = "greater")

# two-sampple t-test ------------------------------------------------------
# creating a new dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
# visualisation
hist(r_two$dat)
# run a basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# now we want to know if sample A is less than sample B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

# now what if we want to know if sample A is bigger
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

# 6,6. A t-test workflow --------------------------------------------------

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(fitdistrplus)
library(logspline)
library(plotly)
library(ggpubr)
library(ggplot2)

# Loading data
ecklonia <- read.csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, ID)

# visualising data gathered in previous line of code (all variable plotted)
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# filter the data, allowing us to work with a selective variable ie. stipe length
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_length")

# visualising filtered data, pertaining only to stipe length at the two sites
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# formulate hypothesis ----------------------------------------------------

# Is there a significant difference between the stipe length of E. maxima at Boulders Beach,
# compared to that of Batsata Rock.

# H0: there is a significant difference between the two.
# H1: there is no significant difference between the two.

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_length_var = as.numeric(shapiro.test(value)[1]),
            stipe_mass_norm = as.numeric(shapiro.test(value))[2])

t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
# The t-test indicates that there is a significant difference 
# between the stipe lengths from the two sites (Batsata Rock mean = 619.1538, 
# Boulders Beach mean = 443.4615)
# the difference is further indicated by the p value displayed below
# t = 4.5187, df = 24, p-value = 7.062e-05
# Therefore we refute H1 and accept H0

# Exercise 1 --------------------------------------------------------------

# setup -------------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(fitdistrplus)
library(logspline)
library(plotly)
library(ggpubr)
library(ggplot2)

# I hypothesise that, pure bred wolves have bigger fangs (cm) than cross bred wolves (werewolves).
# H0: Werewolves do have bigger fangs than wolves.
# H1: Werewolves do not have bigger fangs than wolves.

# creating data -----------------------------------------------------------

# randomised normal data for two species of wolves

data.frame(fang_length = c(rnorm(n = 201, mean = 12, sd = 5), 
                   rnorm(n = 201, mean = 5, sd = 2)), 
           species = c(rep("wolf", 201), rep("werewolf", 201)))

# assigning a name to data set, this allows us to view it in the environment pane
wolfgang <- data.frame(fang_length = c(rnorm(n = 201, mean = 12, sd = 5), 
                               rnorm(n = 201, mean = 5, sd = 2)), 
                       species = c(rep("wolf", 201), rep("werewolf", 201)))

# visualising datasets normality
hist(wolfgang$fang_length)

# visualising dataset using a box plot
ggplot(data = wolfgang, aes(x = species, y = fang_length, fill = species)) +
  geom_boxplot(position = "dodge") +
  labs(y = "Species", x = "Fang length (cm)") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# testing normality of dataset
shapiro.test(wolfgang$fang_length)
# p value = 3.024e-11 (sig diff)

wolfgang %>% 
  group_by(species) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(fang_length)[2]))

wolfgang %>% 
  group_by(species) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(fang_length)[2]),
            r_norm_var = var(fang_length))

# run a basic test
t.test(fang_length ~ species, data = wolfgang, var.equal = TRUE)

# now we want to know if sample A is less than sample B
t.test(fang_length ~ species, data = wolfgang, var.equal = TRUE, alternative = "less")

# now what if we want to know if sample A is bigger
t.test(fang_length ~ species, data = wolfgang, var.equal = TRUE, alternative = "greater")

# basic t-test reflects a p value of 2.2e-16 (p < 0.05),
# therefore we accpt H1. however t-test for "less" and "greater" indicates a p value of 1 
# not quite sure what to make of that.

