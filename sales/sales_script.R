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

# Juneaid's additions:
# This is a pipe that orders the data in sequence and sorts/groups the data based
# on pizza type and month and then gives the total sales each month per pizza type.

# Diego's comments:
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
  
# Jason's comments
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

sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))  
# Juneaid's addition?
# Creates a column named date and pastes the data from three columns into the 
# newly created date column.

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

# Juneaid's comments
sales_summary_daily <- sales_data %>%
  # Beginning of a new pipe after creating a date column that looks at days.
  group_by(pizza, date) %>% 
  # The columns being grouped are pizza and date, so each row of a type of pizza
  # on a given day will be combined into a single row.
  summarize(total_sales = sum(number))
  # The new summary column based on the grouped data is totaling the number of
  # each type of pizza on each day using the sum function on the number column.

# Also Juneaid? Jason
# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))
# This is a pipe that orders the data in sequence and sorts/groups the data based
# on pizza type and date and then gives the total sales per pizza type date-wise.

# Diego's comments
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

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")
  # Junaid's comments:
  # Bar graph showing pizza-type sales date-wise with hawaiian sales starting off
# high on Mar 9 followed by four cheese and hawaiian pizza peak sales on 11 Mar
# which then dips down afterwards.However, four cheese pizza maintains the 
# dominance in sales compared to other pizzas. Bar graph looks more intuitive 
# compared to line graph.

# Kim's additions:
# Average data
# This code creates a new dataframe (summary table), sales_ave_daily, with three
# columns: pizza, date, and mean number of sales by pizza and date. The group_by() 
# function grouped the sales_data by pizza and date, and the summarize() 
# function averages number of sales for each pizza x date combination. The 

# Jinyi's additions:
# Group the sales_data by pizza type and then date. Then calculate the ave_sales,
# which is the mean of the number value.

# Juneaid's comments
sales_ave_daily <- sales_data %>%
  #Beginning of a new pipe looking at average daily instead of total daily sales.
  group_by(pizza, date) %>% 
  # Columns being grouped are the same as the last summary, pizza and date.
  summarize(ave_sales = mean(number))
  # This summary is performing the mean function on the number column for each 
  # type of pizza on each day to find the average number per day per type.

# Diego's comments
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
  
# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

# Jason?
ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
  # Juneaid's comments
# Line graph showing pizza-type sales date-wise with margherita sales starting off
# high on Mar 9 followed by four cheese and margherita pizza peak sales on 11 Mar
# which then dips down afterwards.
