# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# The codes provided by the lines 13 to 15 give an output (graph) of the monthly
# sales per pizza type.
# In order to get that result it was created a nested function. First, it was 
# created a new object called "sales_summary"  where we used the data frame
# "sales_data, grouped the variables "pizza" and "month" and made a sum of the 
# total pizzas sold called here as "total_sales.

# The pipes are designed to be used in this nested functions, where the result
# of one function becomes the argument for the next function. 
# Monthly sales per pizza type   
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

# Here we are using ggplot to make a bar plot of sales_summary, where the x axis
# it the variable pizza and y axis the total sales.
# It is possible to identify that margheritta pizza had the highest sales for
# pizza of the month.
ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Here, it was also created a nested function very similar with the one above 
# using sales_data , but instead of grouping by pizza type and month, 
# it grouped by day, that is, sales made from mar 09 to 13. 
# It used the summarize function here as well, and it counted total_sales
# as the sum of the column "number".
# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Here we got a plot of the just created "sales_summary_daily". With date 
# from mar 09 to 13 on the x axis and total_sales on the y axis. 
# It is possible to identify that margheritta and vegetarian were highly 
# consumed on march 11.
# on the bar plot, it is easier to visualize and identify the consumption 
# of each type of  pizza per day, given here by color. 
#  
# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")


# Here, it was created another nested function, where sales_data was used again,
# it grouped by pizza and date, however instead of getting the sum of the 
# total sales, it gets the mean of the column number, that is, the mean of 
# the total of sales per day for each pizza type.
# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

# It gives a bar plot with the results from the nested functions above.
# Our data here is sales_ave_daily, with date on the x axis and ave_sales on the 
# y axis, and it is filled the bars with the variable pizza. 
# From the average sales perceptive, all pizzas types vary each day, but
# margheritta seems to be one of the most promising ones, especially on mar 09. 
ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
