# Day_2.R
# To practice some of the concepts we will explore
# 13 April 2018
# Second day of Biostats with Robbie, data visualisations and distribution


# Setup -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(e1071)
library(dplyr)

# mean, median, variance and standard deviation? ---------------------------


# to make random data (normal) use rnorm() function
r_data <- data.frame(dat = rnorm(n = 700, mean = 372, sd = 50), 
                     sample ="A")

# visualising our random data, to check if it is what we sort of need 
ggplot(data = r_data, aes(x = dat)) +
  geom_density()

# calculating mean manually
# sum of all points / number of all points

r_data %>% 
  summarise(r_sum = sum(dat),             #sum of all points
            r_n = n(),                    #sum of number of samples
            r_mean = r_sum/r_n,           #mean manual
            r_mean_func = mean(dat))   

# calculating median manually
#subset data set (n+1)/2
# use slice() function to select specific row of data

r_data$dat[dat(length(r_data$dat))] 


# we need to order (arrange) data before calculating median manually
# using tidy
r_data %>% 
  arrange(dat) %>% 
  slice(n()/2)

# tidy automagic way
r_data %>% 
  summarise(r_median = median(dat))


# Variance manually
# sum of individual values minus the mean squared / sample count minus 1

r_data %>% 
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>%  # use mutate function to add column on existing dataframe
  summarise(r_squared_sum =sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            r_var_func = var(dat))   # simply using R function

# standard deviation manually
r_data %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))   #simply using R function


# Exercise 1 --------------------------------------------------------------
# getting measures of central tendancy from a dataframe

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart1 = quantile(weight, 0.25),
            median_weight = median(weight),
            mean_weight = mean(weight),
            quart3 = quantile(weight, 0.75),
            max_weight = max(weight))


# Chapter 4 ---------------------------------------------------------------

# setup -------------------------------------------------------------------
library(ggpubr)
library(RColorBrewer)
library(viridis)

# publication ready figures

# Qualitative -------------------------------------------------------------

# stacked bar graphs

# load sa time data
sa_time <- read.csv("sa_times")

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("cape town", "george", "PE"), times = 6),
                 rep("joburg", 2))) #adding additional human row to raw data set

#creating long data
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)

sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n))

# stacked bar proportions graph
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "Relative proportions", 
       x = NULL, y = "proportions") +
  theme_minimal()

# stacked bar graph
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "stacked bar graph", subtitle = "cumulative sum", 
       x = NULL, y = "count") +
  theme_minimal()

#pie chart (user discretion is advised)
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie Chart", subtitle = "But why though?", 
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()

sa_clean <- sa_long %>% 
  filter(minutes < 300)

# a faceted histogram 1 (side * side)
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, scales = "free_x")

# a faceted histogram 2 (on top of one another)
# add ncol = 1, to stack
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# shows relative proportions
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), 
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# we now know that overall, 
# people have their own way of defining now, just now, and now now
# ask familia about their choices



# box plots ---------------------------------------------------------------

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

# boxplots are visualisation of summarise function
# 3rd quart = top line of box
# 1st quart = bottom line of box
# median = thick line between box
# tails are produced by taking box and multiplying it by 1.5

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

# summary stats for plotting over boxplot
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")


# Relationships -----------------------------------------------------------

# making a basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,300), ylim = c(0,300))

# limiting our scale view
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,60), ylim = c(0,60))

# adding trending lines
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0,60), ylim = c(0,60))
# grey areas represent variance around data

















