# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>% ###pipe takes the output of sales_data and 
                                      ###use this output as an argument in another function
  group_by(pizza, month) %>%   ### it groups the dataframe by given column name, 
                                  ## Here, it will group the sales data by pizza and month
  summarize(total_sales = sum(number))    ###It calculates the sum of each pizza type in the 
                                  ###given month ( 3, grouped by above code) and puts that in the col (total sales)
ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")   ###creates the bar chart of the summary table for pizza and total sales 
   ##with total sales as an dependent y variable provided in the ggplot function.  
  ##otherwise Geom_bar by default will count the no.of obs based on the x-variable(pizza) grouping.

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

###1st line; pipes the sales data and stores in variable sales_summary_daily
###2nd line: groups the sale of pizza type  by date
##Calculates the total number of sales of pizza each day 


# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))
###Groups the sales by pizza type and date
### Calculates the mean of the number of each sales in the column ave_sales.

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
