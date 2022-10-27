# Pizza sales
rm(list = ls())
##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######

# create new sales data and export to sales folder
day <- rep(1, length.out = 20)
month <- rep(12, length.out = 20)
year <- rep(2022, length.out = 20)
pizza <- rep(c("margherita","hawaiian","meat lovers","vegetarian","four cheese"),length.out = 20)
number <- sample(1:5, 20, replace = T)
date <- ymd(paste(year,month,day, sep = "-"))

new_sales_data <- data.frame(day, month, year, pizza, number, date)
#write.csv(new_sales_data, "sales/202212_sales_CAYETANO.csv")

sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% # groups data by pizza and month columns
  summarize(total_sales = sum(number)) # Sums number of sales per pizza type per month

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% # groups by pizza and date columns
  summarize(total_sales = sum(number)) # Sums number of sales per pizza type per date

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% # groups by pizza and date columns
  summarize(ave_sales = mean(number)) # Averages number of sales per pizza type per date

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
