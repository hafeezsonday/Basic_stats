# Day 5.R with Rob and AJ
# Hafeez Sonday
# April, 20th 2018
# Correlaton, regression and some ANOVA's

# Setup -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(Rmisc)
library(ggpubr)

# Load data ---------------------------------------------------------------

# how long do snakes become acclimatised to a new stressful environment (6snakes over 4 days)
# do snakes eventually become acclimatised to box opening and closing fewer times at day 4 that day 1
snakes <- read.csv("snakes.csv") %>% 
  mutate(day = as.factor(day)) #can also use the following (snakes$day = as.factor(snakes$day))
# the independence assumption of the ANOVA is being vialated

# Manipulate the data -----------------------------------------------------
# we grouped incorrectly
snakes_summary <- snakes %>% 
  group_by(day, snake) %>% #how many times each snake responds in a day but we only have one snake occuring once each day (independence vialation)
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings))
View(snakes_summary) 

# proper way of grouping
snakes_summary <- snakes %>%
group_by(day) %>% #so we group only by day now
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings))
View(snakes_summary)

# Formulate hypothesis ----------------------------------------------------

# H0: there is no difference in the number of openings from day to day
# H1: there is a difference in the number of openings from day to day

# Test hypothesis ---------------------------------------------------------

# first calculate SE and CI
snakes_summary2 <- summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))
View(snakes_summary2)

# visualising our snakes data along with the summarySE data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes_summary2, 
               aes(x = day, xend = day, 
                   y = openings - ci, 
                   yend = openings + ci, 
                   colour = day), 
               size = 2.0, 
               linetype = "solid", 
               show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.05)

# two factors per snakes
# effect of non independence how to test. 
# use ANOVA to test significance


# H0: There is no difference between snakes with respect to the number of 
 #openings at which they habituate.
# H0: There is no difference between days in terms of the number of openings
 #at which the snakes habituate.


# ANOVA -------------------------------------------------------------------

snakes_day_aov <- aov(openings ~ day, data = snakes)
summary(snakes_day_aov)

# we reject the H0: There is no difference between days in terms of the number of openings
# at which the snakes habituate. 
 # because there is a stat sig diff indicated by a p value less than 0.05

# t-test both hypotheses --------------------------------------------------
snakes_all_aov <- snakes_day_aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes_day_aov)
# to test non independence we make snakes as part of the analysis to test 
 # their individual effect on the opening and closing indicated by a p val o0.338

# testing assumptions afterwards ------------------------------------------

# first visualise normality of results 
snakes_residual <- residuals(snakes_all_aov)
hist(snakes_residual)

# then visualise homoscedasticity of results
plot(fitted(snakes_all_aov), residuals(snakes_all_aov))

# check tukey results
snakes_tukey <- TukeyHSD(snakes_all_aov, which = "day")
plot(snakes_tukey)
# visualise factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings, 
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)


# exercise 1 --------------------------------------------------------------

moths <- read.csv("moth_trap.csv") %>% 
  gather(key = "trap", value = "count", -Location)



moth_summary <- moths %>%
  dplyr::group_by(Location) %>% 
  dplyr::summarise(mean_scents = mean(count),
            sd_scents = sd(count))
           
View(moth_summary)

moth_summarySE <- summarySE(data = moths, 
                            measurevar = "count", 
                            groupvars = "Location")
View(moth_summarySE)  

plot1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moth_summarySE, 
               aes(x = Location, xend = Location, 
                   y = count - ci, 
                   yend = count + ci, 
                   colour = Location), 
               size = 3.0, 
               linetype = "solid", 
               show.legend = F) +
  geom_boxplot(aes(fill = Location), notch = TRUE, alpha = 0.6, show.legend = F) +
  geom_jitter(width = 0.5)


moths_aov <- aov(count ~ Location, data = moths)
summary(moths_aov)

moths_all_aov <- aov(count ~ Location + trap, data = moths)
summary(moths_all_aov)

moths_residual <- residuals(moths_all_aov)
hist(moths_residual)

plot(fitted(moths_all_aov), residuals(moths_all_aov))

moths_tukey <- TukeyHSD(moths_all_aov, which = "Location")
plot(moths_tukey)

ggplot(data = moths, aes(x = as.numeric(Location),
                          y = count, 
                          colour = trap)) +
  geom_boxplot()
  # geom_line(size = 3) +
  # geom_point(size = 4)

plt1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap)) +
  geom_jitter(width = 0.05, shape = 21)

plt2 <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plt3 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

ggarrange(plt1, plt2, plt3, nrow = 2 , ncol = 2, labels = "auto")


# Regression --------------------------------------------------------------

# data = eruption from ol' faithful

head(faithful)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = "seagreen")
  # scale_x_continuous(limits = c(0, 100)) +
  # scale_y_continuous(limits = c(-2, 6))

# Form hypothesis ---------------------------------------------------------

# H0: waiting time does not influence duration of an eruption
# H1: waiting time does influence duration of an eruption

# test hypothesis ---------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful) # lm is linear model func
summary(faithful_lm)



# Correlation -------------------------------------------------------------

# Setup -------------------------------------------------------------------
library(corrplot)
library(ggpubr)

# Load data ---------------------------------------------------------------

ecklonia <- read.csv("ecklonia.csv")

# Formulate hypothesis ----------------------------------------------------

# H0: there is no relationship between primary blade width and primary blade length
 # for E. maxima
# H1: there is relationship between primary blade width and primary blade length
 # for E. maxima

# Test hypothesis ---------------------------------------------------------

cor.test(ecklonia$primary_blade_width, ecklonia$primary_blade_length)

# data illustration
ggplot(data = ecklonia, aes(x = primary_blade_length, y = primary_blade_width)) +
  geom_point()

# Hecka test --------------------------------------------------------------
ecklonia_sub <- ecklonia %>%
  select(stipe_length:stipe_mass) # selects specific columns in a dataset

ecklonia_cor <- cor(ecklonia_sub)
View(ecklonia_cor)

# Spearman test -----------------------------------------------------------
# ordinal column
ecklonia$length <- as.numeric(cut((ecklonia$primary_blade_length + ecklonia$primary_blade_width), 3))
# cut create bins where each one belongs to a certain incriment

# run spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")

# Kendall rank test -------------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# illustration of all things since the dawn of time -----------------------
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson
corrplot(ecklonia_pearson, method = "circle")


# Heatmap -----------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(dplyr)


library(reshape2)
melted_ecklonia <- melt(ecklonia_pearson)

ggplot(data = melted_ecklonia, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "steelblue", high = "yellow")
