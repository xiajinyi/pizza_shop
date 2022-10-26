# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>% 
  #The creation of a new table/dataframe, using a pipe flow
  group_by(pizza, month) %>% 
  #Taking a group of data, the pizza and month columns, from sales_data
  # continuing the pipe, combining all matching data of the groups into single
  # rows. In this case, each type of pizza per month gets its own row.
  summarize(total_sales = sum(number))
  # Summarizing the two pizza and month columns with the creation of a new 
  # column, total_sales, that sums all the data in the number column that 
  # corrresponds to the grouped data of pizza/month.

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>%
  # Beginning of a new pipe after creating a date column that looks at days.
  group_by(pizza, date) %>% 
  # The columns being grouped are pizza and date, so each row of a type of pizza
  # on a given day will be combined into a single row.
  summarize(total_sales = sum(number))
  # The new summary column based on the grouped data is totaling the number of
  # each type of pizza on each day using the sum function on the number column.

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  #Beginning of a new pipe looking at average daily instead of total daily sales.
  group_by(pizza, date) %>% 
  # Columns being grouped are the same as the last summary, pizza and date.
  summarize(ave_sales = mean(number))
  # This summary is performing the mean function on the number column for each 
  # type of pizza on each day to find the average number per day per type.

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
