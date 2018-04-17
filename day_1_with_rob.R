# Day_1.R
# To practice some of the concepts we will explore
# 12 April 2018
# First day of Biostats with the Rob and AJ duet (Git things) 


# Setup -------------------------------------------------------------------

library(tidyverse)


# Integers ----------------------------------------------------------------

# Generating integer data

Integer_r <- as.integer(seq(5, 14, by = 1)) 

# look at brief summary of them

summary(Integer_r)

# Continuous --------------------------------------------------------------

# Generate seq of numeric values
Numerica_r <- seq(23, 43, length.out = 10)

# Dates -------------------------------------------------------------------

# performing arithmetic with dates
as.Date("2005-12-13") - as.Date("2005-12-12")
# or
Dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
 summary(Dates_r)

# Dataframe ---------------------------------------------------------------

# create base dataframe
df_r <- data.frame(integers = Integer_r, numeric = Numerica_r, dates = Dates_r)

#upgrade to a tibble
df_r <- as_tibble(df_r)
summary(df_r) 

# Categories --------------------------------------------------------------

# electronics
elec_r <- as.factor(c("laptops", "desktops", "cellphones"))

# people
people_r <- as.factor(c("funny", "beautiful", "beanies"))
?as_factor


# Ordinal data ------------------------------------------------------------
# factor data with order of preference
# we still have qualitative data
#but with some sort of order

colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"), 
                       levels = c("blue", "green", "yellow", "orange", "red"))

# Binary ------------------------------------------------------------------

#generally presented as TRUE or FALSE

binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r) 

# Characters --------------------------------------------------------------

sites_r <- c("ystervarkpunt", "Betty's Bay", "Gansbaai", "Sea point")
summary(sites_r) 

# C arguement means combine


# Missing values ----------------------------------------------------------

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
summary(chicks_nest) 


# Viewing data ------------------------------------------------------------

# RWS: You can't call the chicks dataframe if you haven't created it yet
head(ChickWeight, 5)
# choosing specific values from a column of a particular dataset
ChickWeight[c(1, 54, 61, 12), 2]

# Descriptive statistics --------------------------------------------------

#create dataframe
chicks <- as_tibble(ChickWeight)

# count the data
chicks %>% 
  summarise(n())

# Measures of central tendancy --------------------------------------------

# calculate mean weight
chicks %>% 
  summarise(mean_wt = mean(weight))

# being more specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight)) # mean come become corrupted with many outliers then use median

# visualise density of data
ggplot(data = filter (chicks, Time == 21), 
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)

# Skewness ----------------------------------------------------------------

# calculate numeric value
# loading library...
library(e1071)

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight), 
            skew_wt = skewness(weight))

# Kurtosis ----------------------------------------------------------------

# calculating the kurtosis of the tails of distribution
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight), 
            skew_wt = skewness(weight), 
            Kurtosis_wt = kurtosis(weight))

# Measures of variability ----------------------------------------------------------------

#below is a summary of many different statistical properties

wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight), 
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight), 
            wt_min = min(weight), 
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.50), 
            wt_quart3 = quantile(weight, 0.75))

# visualise
wt_summary






