# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

##### 2. Load data #######
sales_data <- read.csv("202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
## This summary shows the amount of pizzas of each pizza type was sold in March.
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates

## This code uses lubridate to transform the day/month/year columns into a date 
## column that is more analyze-able by R. 
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
## This is a line graph plots the number of each pizza type sold throughout March 

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

## This is the same information as the previous graph, except communicated in a 
## bar graph. 

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

## This plots the average number of each pizza type sold each day. 

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
