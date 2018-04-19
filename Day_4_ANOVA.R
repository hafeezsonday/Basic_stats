# Day 4 with the Rob and Smit duo on ANOVA
# Hafeez Sonday
# April, 19th 2018
# Playing around with ANOVA analysis usuing chicken data


# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)


# Load data ---------------------------------------------------------------
# grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #combine func (c) is diff to gather (makes tidy data)

# t-test checking wherethe or not there is a sig diff between diets one and two
t.test(weight ~ Diet, data = chicks_sub)

# one-way ANOVA -----------------------------------------------------------

# Research Q: Is there difference in chicken mass at day 21 as a result of 
# them being fed 4 different diets?

# Null hypothesis: there is no difference in chicken mass at 21 days after having fed one of 
# four diets.

chicks_21 <- chicks %>% 
  filter(Time == 21)  # here we filter out the time in which we are interested in

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# Pr(>F) = 0.00686, therefore we reject the null hypothesis

# this visualisation displays for us the differences in weight  at time 21 per diet 
ggplot(data = chicks_21, aes(x = Time, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE)

# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)

# tukey test indicates that on diets 3 and 1 have a sig diff from one another

# When Robbie took over
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# segment
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2)) +
  coord_flip()

#
chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))

ggplot(data = chicks_tukey) +
  geom_errorbar(aes(x = pairs, ymin = lwr, ymax = upr)) + 
  coord_flip() +
  labs(y = "Difference in mean levels of diet", x = "Diet pairs") +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))



# Multiple factor ANOVa ---------------------------------------------------

# libraries already loaded
# H0: There is no change in chicken mass (kg) from day 0 to day 21

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21)) # %in% allows R to discared all other times and only use 0,2, and 21

# visualising data
ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time))) #as.factor makes a continuous variable into a category

#Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21)) #squiggly line means formular weight as a func of time

# perform a Tukey
TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))

# 95 % family-wise confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

# looking only at day 0 and day 21 for both time and diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# or simply look at all of the time
# Not the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# note the increase in the degrees of freedom for the time factor
# but no increase for the d.f.  for diet

# interaction between factors (rep by * sign in formular (~))
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# tukey results for above line of code
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# visualising concept as to help explain it and understand it
# first create mean value by time and diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

#then visualise it
ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 1.5) +
  geom_point(shape = 10, size = 4.5)


# Non-parametric test -----------------------------------------------------

# what if we do not have norml data (many outliers and shizz)?
# we use a wilcox rank sum test instead of t-test
wilcox.test() #fill in this func same way as we did the t-test 


# kruskall wallis test
kruskal.test(weight ~ Diet, data = chicks_0_21)

# load this for non-parametric post-hoc test
library(pgirmess)

kruskalmc(weight ~ Diet, data = chicks_0_21)
