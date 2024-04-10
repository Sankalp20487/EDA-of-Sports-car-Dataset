## Name - Sankalp Susil Kumar Biswal Date - 10/07/2023 Class - ALY6000 Project-4

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
library(ggplot2)
library(tidyverse)
library(janitor)
library(lubridate)
install.packages("fmsb")
library(fmsb)
install.packages("ggrepel")
library(ggrepel)
#Project4 ----
##Importing Dataset
car_data <- read.csv("Sport car price 2.csv")

##Cleaning Dataset ----

#Standardizing column names for consistency
car_data <- clean_names(car_data)

# Changing few column names for better readability and understanding
# Using rename() function 
car_data <- car_data%>%
  rename(
    Zero_to_60mph_time_seconds = x0_60_mph_time_seconds,
    engine_size_litres = engine_size_l
    )
# Determining the data type for each feature
str(car_data)

#Assigning dataset to a new variable for working
car_data_new <- car_data

# Now we'll convert data type of cols like horsepower,Zero-60mph time, torque and price but before that
# We will remove special characters like '+','-','>','<',etc. so that we don't
# get NA values after conversion
# Using trimws() and gsub() for removing special characters

car_data_new$horsepower <- trimws(gsub('+|,','',car_data_new$horsepower,fixed=T))

car_data_new$torque_lb_ft <- trimws(gsub('+|-|,','',car_data_new$torque_lb_ft,fixed=T))

car_data_new$price_in_usd <- gsub(',','',car_data_new$price_in_usd,fixed=T)

# Converting following cols to numeric datatype since they where 'chr' previously and that
# could lead to problems while applying mathematical functions

car_data_new$price_in_usd <- as.numeric(car_data_new$price_in_usd)

car_data_new$torque_lb_ft <- as.numeric(car_data_new$torque_lb_ft)

car_data_new$horsepower <- as.numeric(car_data_new$horsepower)

car_data_new$Zero_to_60mph_time_seconds <- as.numeric(car_data_new$Zero_to_60mph_time_seconds)

# Removing NA values from the table
car_data_new <- na.omit(car_data_new)


#Ques1 What is the correlation between engine size and power?----
#We need to remove electric cars and hybrids from this comparison since engine_size is not applicable to them

non_electric_car$engine_size_litres <- as.numeric(non_electric_car$engine_size_litres)

#After applying the above, all the fields with "Electric","Hybrid", "N/A" have been converted into n/a
#Removing these na values

non_electric_car <- na.omit(non_electric_car)

##Scatter plot #1 for Engine size vs Horsepower----

ggplot(non_electric_car, aes(x = engine_size_litres, y = horsepower, color= horsepower)) +
  geom_point() +
  labs(x = "Engine Size", y = "Horsepower") +
  ggtitle("Correlation between Engine Size and Horsepower") +
  geom_text(x = 2.5, y = 1400, label = paste("Correlation: ", round(correlation, 2)))
correlation <- cor(non_electric_car$engine_size_litres, non_electric_car$horsepower)

##Scatter plot #2 for Engine size vs Torque----

ggplot(non_electric_car, aes(x = engine_size_litres, y = horsepower, color= torque_lb_ft)) +
  geom_point() +
  labs(x = "Engine Size", y = "Torque") +
  ggtitle("Correlation between Engine Size and Torque") +
  geom_text(x = 2.5, y = 1400, label = paste("Correlation: ", round(correlation, 2)))
correlation <- cor(non_electric_car$engine_size_litres, non_electric_car$torque_lb_ft)


#Ques2 What is the average price of a performance car for each car make?----
# Which car offers the best price/performance ratio? 

# Filter for performance cars (you can define your criteria)
budget_threshold <- 100000  # Set your budget threshold
high_end_threshold <- 300000  # Set your high-end threshold

# Create a new column "Price_Category" based on the thresholds
car_data_new <- car_data_new %>%
  mutate(
    Price_Category = case_when(
      price_in_usd <= budget_threshold ~ "Budget",
      price_in_usd > budget_threshold & price_in_usd <= high_end_threshold ~ "Medium",
      price_in_usd > high_end_threshold ~ "High-End"
    )
  )

# Creating column for Price to performance ratio

car_data_new <- car_data_new%>%
  mutate(Price_to_Performance= horsepower/price_in_usd)
# Creating subset for budget cars

budget_cars <- car_data_new%>%
  filter(car_data_new$Price_Category == 'Budget')
#Filtering cars with best Price_to_performace ratio
budget_cars<- arrange(budget_cars, desc(Price_to_Performance))

#Slicing top cars
budget_cars_top<- slice(budget_cars, 1:150)

# Calculate the average price for each car make
avg_price_by_make_budget <- budget_cars_top %>%
  group_by(car_make) %>%
  summarise(
    Avg_Price = mean(price_in_usd, na.rm = TRUE)
    )


# Create a bar plot using ggplot2
ggplot(avg_price_by_make_budget, aes(x = Avg_Price, y = reorder(car_make, Avg_Price), fill = car_make)) +
  geom_bar(stat = "identity") +
  labs(x = "Average Price", y = "Car Make") +
  ggtitle("Average Price of Performance Cars by Car Make") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme_gray()


#Ques3 How do top 4 performance cars stack up against each other in terms of price, power, acceleration? ----

top_performance_cars <- car_data_new
top_performance_cars <- filter(car_data_new, Zero_to_60mph_time_seconds<2.5)
top_performance_cars<- arrange(top_performance_cars, Zero_to_60mph_time_seconds)

best_performance_cars <- top_performance_cars %>%
  group_by(car_make) %>%
  summarise(
    accel_time = min(Zero_to_60mph_time_seconds),
    price = mean(price_in_usd),
    power = max(horsepower)
  )

best_performance_cars <-arrange(best_performance_cars,accel_time)
##Getting the top 4 performance cars
top_4_performance_cars <- slice(best_performance_cars, 1:4)

## Create a Bubble Chart using ggplot2
ggplot(top_4_performance_cars, aes(x = price, y = power, size = accel_time, label = paste(car_make, accel_time), color = car_make)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(4, 15)) +
  labs(
    title = "Bubble Chart of Top 4 Performance Cars ",
    x = "Price",
    y = "Power",
    size = "Acceleration Time"
  ) +
  theme_get() +
  geom_text_repel(size = 3, color = "black")  

#Ques4 What is the trend of acceleration time?----

# Create a Histogram using Histogram using ggplot2
ggplot(car_data_new, aes(x = Zero_to_60mph_time_seconds)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Acceleration time Trend",
    x = "Acceleration_Time_Seconds",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none") +
    scale_x_continuous(
      breaks = seq(0,8, by=0.75),
      limits = c(0,8))
    
  
