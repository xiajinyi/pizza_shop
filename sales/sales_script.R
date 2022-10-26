# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
sales_data <- read.csv("sales/202003_sales.csv")

##### 3. Create summaries #####

# Monthly sales per pizza type
sales_summary <- sales_data %>% # In this line we are assigning all the code to 
  # a new object called sales_summary, we started with the dataset sales_data, 
  # then we used pipes to move forward with this dataset. When we see the pipes 
  # symbol %>% it means that we are doing a sequence of actions. 
  group_by(pizza, month) %>% # Here we use group_by() to put the observations 
  # in groups, and the following operations will be performed "by group". 
  # Because we put two arguments inside the function, this will group both by 
  # the pizza type, and month. (then pipes again)
  summarize(total_sales = sum(number)) # The function summarise() creates a new 
  # dataframe, it will have one (or more) rows for each combination of grouping 
  # variables. In this case, because we said sum(number), this column called 
  # "total_sales" will show the sum of the elements in the "number" rows for each 
  # of the groups above. The result will allow us to see the total of each 
  # flavor of pizza sold and which month that was sold. It is worth to note 
  # that after running this, the console pane returns the following message: 
  # `summarise()` has grouped output by 'pizza'. You can override using the 
  #`.groups` argument.
  # I checked on Google and found the following about it: The reason for the 
  # message is that the dplyr package drops the last group variable that was 
  # specified in the group_by function, in case we are using multiple columns to 
  # group our data before applying the summarise function. This message helps to 
  # make the user aware that a grouping was performed. However, the message does 
  # not have an impact on the final result.

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  

# Summarize data
sales_summary_daily <- sales_data %>% # Once again creating a new object, 
  # selecting our dataset, and using pipes to establish a sequence of actions. 
  group_by(pizza, date) %>% # We are doing the same thing we did in question 3, 
  # but now the second group will be the actual date, which now has its columns 
  # that was created in the previous step. (then pipes again)
  summarize(total_sales = sum(number)) # This is very similar to what we did 
  # before, but here we are working with the dates and not months. The result 
  # is a dataframe where we'll see all the pizza flavors, the day they were sold, 
  # and how many pizzas were sold that day.

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily2 <- sales_data %>% # Once again, this is similar to what was done 
  # before, new object <- dataset %>% (pipes to create sequence),
  group_by(pizza, date) %>% # function group_by() to group variables together, 
  # then pipes to keep the sequence. 
  summarize(ave_sales = mean(number)) # This will result in an average of how 
  # many pizzas were sold that day. Checking the original dataset, I saw that 
  # there are multiple entries for the same day and same pizza. For example, 
  # for the Hawaiian pizza, there are 4 different rows but all are from the same 
  # day. Even though this is the case, each of this rows for Hawaiian has a 
  # different amount of pizzas sold (number column), so the mean here is the 
  # average of the pizzas sold on that day. This is taking all the repetitive 
  # dates, and making an average from that. 

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
