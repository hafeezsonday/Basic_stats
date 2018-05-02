# Hafeez Sonday
# Exercises at the end of the ANOVA chapter
# April 26th, 2018
# ANOVA exercises.R

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(dplyr)

# Load data ---------------------------------------------------------------

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
cow <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# Question ----------------------------------------------------------------

# Does feed type have a significant effect on cows final weight at
 # the end of the experiment?

# Hypothesis --------------------------------------------------------------

# H0: different feed types will not have a significant effect on cows final
 # mass.
# H1: different feed types WILL have a significant effect on cows final
 # mass.

# Test hypothesis ---------------------------------------------------------

cow_aov <- aov(mass ~ feed, data = cow)
summary(cow_aov)

# the ANOVA summary analysis spat out a p value of < 0.05 
 # this tells us that there is a sig diff when mass is a function of feed
  # so basically feed does influence mass.

# Visualising our data ----------------------------------------------------
#producing boxplot

ggplot(data = cow, aes(x = feed, y = mass, fill = feed)) +
  geom_boxplot(notch = TRUE)
# the sig diff is clearly illustrated by a lack of overlaps with regards
 # to the boxplots. However, this only tells us that there is a general
  # sig diff between diets.

# Tukey analysis ----------------------------------------------------------

# this will indicate which diets specifically are sig diff from one another
TukeyHSD(cow_aov)
# so the tukey analysis compares the differenece between feeds, which in
 # produces the adjusted p values, and for each difference between feeds
  # it indicates p values < 0.05

# visualise the TukeyHSD test ---------------------------------------------

plot(TukeyHSD(cow_aov))
# here we see that all diets are sigg diff from one another.
# therefore we reject the null hypothesis

# Exercise 2 --------------------------------------------------------------

# Question ----------------------------------------------------------------
# Do different doses of Vitamin C have an effect on the growth of teeth
 # length?

# Hypothesis --------------------------------------------------------------
# H0: different doses of Vitamin C will have NO effect on the growth
  # of teeth length
# H1: different doses of Vitamin C will have an effect on the growth
 # of teeth length

# toothgrowth data
# Load data ---------------------------------------------------------------
teeth <- datasets::ToothGrowth

# filter out Vitamin C dosage data.
teeth_vit.c <- ToothGrowth %>% 
  filter(supp == "VC")

# ANOVA test --------------------------------------------------------------
teeth_aov <- aov(len ~ as.factor(dose), data = teeth_vit.c)
summary(teeth_aov)
# the ANOVA analysis reveals that there is a stat sig diff, this is 
 # indicative by a P value <0.05.
# therefore we reject the null hypothesis and accept the 
 # alternative hypothesis.

# Visulaise the sigg diff using a boxplot ---------------------------------
ggplot(data = teeth_vit.c, aes(x = as.factor(dose), y = len, 
                            fill = as.factor(dose))) +
  geom_boxplot(notch = TRUE)
# the boxplot is indicates no overlapping between boxes, 
 # therefore we can assume that all doses have varying effects on 
  # teeth length.

# to make sure this is the case we can perform a tukey analysis.

# Tukey analysis ----------------------------------------------------------

TukeyHSD(teeth_aov)

# Visualise Tukey test ----------------------------------------------------

plot(TukeyHSD(teeth_aov))
# so the tukey analysis compares the differenece between vitamin C dosages, 
 # which in turn produces the adjusted p values, and for each difference 
  # between vitamin c dosages it indicates p values < 0.05

# Exercise 3 --------------------------------------------------------------

teeth <- datasets::ToothGrowth

# H0: The interaction between supplement and dose has no effect on 
 # tooth length
# H1: The interaction between supplement and dose does have an effect on 
 # tooth length

# length by supplement
summary(aov(len ~ supp, data = teeth))
# P > 0.05

# Tukey analysis ----------------------------------------------------------

TukeyHSD((aov(len ~ supp, data = teeth)))
# P > 0.05

# Visualising the Tukey analysis ------------------------------------------

plot(TukeyHSD((aov(len ~ supp, data = teeth))))

# dosage was already explored in ex 2, so attampting to do more anova
 # analyses will increase error margin.

# Interactions BETWEEN factors
summary(aov(len ~ supp * as.factor(dose), data = teeth)) 
# so the summary code indicates that the p value for factor alone is < 0.05,
 # however supplement and dose as a unit has no effect, indicative of 
  # a p value > 0.05

# So which one has the effect?
TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))))
# On plot, all combinations of supp and dose have a sigg effect on tooth
 # length, those that cross the zero line have no effect.

