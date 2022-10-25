# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")
dim(sales_data) # 62 rows and 5 columns
str(sales_data) # integer, character data
summary(sales_data) # describes min, median, etc. of each column
View(sales_data) # view data and skim for any NAs etc.

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>% # calls data from sales_data to create new
  group_by(pizza, month) %>%  # groups pizza by month (ties each data point of pizza to the 
                              # month it was purchased in)
  summarize(total_sales = sum(number)) # creates total_sales as y-axis and input is the sum
                                      # of the number of pizza sold

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")
# makes x-axis pizza (which is now grouped by month) and y-axis total sales 
# (total sum of the number of types of pizza for that month)

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))
# creates a new column in sales_data specifying day, year, and month within each date.

# Summarize data
sales_summary_daily <- sales_data %>% # creating summary of daily sales from sales_data
  group_by(pizza, date) %>% # grouping pizza by date 
  summarize(total_sales = sum(number)) # creates total_sales as y-axis and input is the sum
                                      # of the number of pizza sold

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()
# line plot of specific dates and total sales. Data/color is filled in by type of pizza.


ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")
# bar chart of specific dates and total sales. Data/color is filled in by type of pizza.
# In my opinion, this is harder to read than the preceding line plot of the same data. 
# I played around with color and fill = with both of these plots and saw that if you fill=
# the line plot, all lines are gray and the key with the pizzas is gone. In using color= 
# in the bar graph, an outline of color is provided instead of a filled-in bar graph.

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
# bar chart of specific dates and average sales per day. Data/color is filled in by type of pizza.
