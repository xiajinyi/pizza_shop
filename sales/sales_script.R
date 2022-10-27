# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202201_sales_MedelinKant.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
# Here we are summarizing the pizza sales by summing up the total
# sales for each pizza type by month using a pipe 
# We use sales_data, group by pizza type and the month it was sold,
# and sum the total number of pizza sold of each type by month sold.
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
# Here we are combining the year, month, and day columns into one date vector
# with the proper data type of date instead of integer and saving over our old data object
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")

