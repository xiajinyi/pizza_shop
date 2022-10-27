# Juneaid's additions:
# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))
# This is a pipe that orders the data in sequence and sorts/groups the data based
# on pizza type and month and then gives the total sales each month per pizza type.

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  
# Creates a column named date and pastes the data from three columns into the 
# newly created date column.

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))
# This is a pipe that orders the data in sequence and sorts/groups the data based
# on pizza type and date and then gives the total sales per pizza type date-wise.

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()
# Line graph showing pizza-type sales date-wise with margherita sales starting off
# high on Mar 9 followed by four cheese and margherita pizza peak sales on 11 Mar
# which then dips down afterwards.

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")
# Bar graph showing pizza-type sales date-wise with hawaiian sales starting off
# high on Mar 9 followed by four cheese and hawaiian pizza peak sales on 11 Mar
# which then dips down afterwards.However, four cheese pizza maintains the 
# dominance in sales compared to other pizzas. Bar graph looks more intuitive 
# compared to line graph.

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))
#

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")


#######################################

# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####
# Kim's additions:
# Monthly sales per pizza type
# This code creates a new dataframe (summary table), sales_summary, with three
# columns: pizza, month, and total sales by pizza and month. The group_by() 
# function grouped the sales_data by pizza and month, and the summarize() 
# function summed summed the number of pizzas sold for each pizza x month 
# combination.

# Jinyi's additions:
# Group the sales_data by pizza type and then month. Then calculate the total
# sales for each group, which is the sum of the number value.
# Monthly sales per pizza type

sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Kim's additions:
# Summarize data
# This code creates a new dataframe (summary table), sales_summary_daily, with 
# three columns: pizza, date, and total sales by pizza and date. The group_by()
# function grouped the sales_data by pizza and date (created in the step above),
# and the summarize() function summed the number of pizzas sold for each 
# pizza x date combination.
# Jinyi's additions:
# Group the sales_data by pizza type and then date. Then calculate the total
# sales for each group, which is the sum of number value.

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Kim's additions:
# Average data
# This code creates a new dataframe (summary table), sales_ave_daily, with three
# columns: pizza, date, and mean number of sales by pizza and date. The group_by() 
# function grouped the sales_data by pizza and date, and the summarize() 
# function averages number of sales for each pizza x date combination. The 

# Jinyi's additions:
# Group the sales_data by pizza type and then date. Then calculate the ave_sales,
# which is the mean of the number value.

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")

