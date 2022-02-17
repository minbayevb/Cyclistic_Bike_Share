# Installing required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
# Displaying working directory
getwd()
setwd("C:/Users/minba/OneDrive/Documents/R/projects/Bike_Share_project/datasets")
# Step 1: Collect Data
# Importing all 12 months data
april_2020 <- read.csv("202004-divvy-tripdata.csv")
may_2020 <- read.csv("202005-divvy-tripdata.csv")
june_2020 <- read.csv("202006-divvy-tripdata.csv")
july_2020 <- read.csv("202007-divvy-tripdata.csv")
august_2020 <- read.csv("202008-divvy-tripdata.csv")
september_2020 <- read.csv("202009-divvy-tripdata.csv")
october_2020 <- read.csv("202010-divvy-tripdata.csv")
november_2020 <- read.csv("202011-divvy-tripdata.csv")
december_2020 <- read.csv("202012-divvy-tripdata.csv")
january_2021 <- read.csv("202101-divvy-tripdata.csv")
february_2021 <- read.csv("202102-divvy-tripdata.csv")
march_2021 <- read.csv("202103-divvy-tripdata.csv")
# Step 2: comparing column names of the each file
colnames(april_2020)
colnames(may_2020)
colnames(june_2020)
colnames(july_2020)
colnames(august_2020)
colnames(september_2020)
colnames(october_2020)
colnames(november_2020)
colnames(december_2020)
colnames(january_2021)
colnames(february_2021)
colnames(march_2021)
# As we can see all column names are the same
all_dfs <- list(april_2020, may_2020, june_2020, july_2020, august_2020,
                september_2020, october_2020, november_2020, december_2020, 
                january_2021, february_2021, march_2021)
EditDataFunction <- function(data){
  clean_data <- data %>% 
    mutate(ride_id = as.character(ride_id)) %>%
    mutate(rideable_type = as.character(rideable_type)) %>%
    mutate(started_at = as_datetime(started_at)) %>%
    mutate(ended_at = as_datetime(ended_at)) %>%
    mutate(start_station_name = as.character(start_station_name)) %>%
    mutate(start_station_id = as.integer(start_station_id)) %>%
    mutate(end_station_name = as.character(end_station_name)) %>%
    mutate(end_station_id = as.integer(end_station_id)) %>%
    mutate(member_casual = as.character(member_casual))
  
  return(clean_data)
  
}

clean_list <- lapply(all_dfs, EditDataFunction)
# Concatenating all DataFrames into one big
all_dfs <- bind_rows(clean_list)
colnames(all_dfs)

all_dfs <- all_dfs[, -c(9:12)] # deleting columns 9 through 12

all_dfs <- rename(all_dfs,
                trip_id = ride_id,
                bikeid = rideable_type, 
                start_time = started_at,  
                end_time = ended_at, 
                from_station_name = start_station_name, 
                from_station_id = start_station_id, 
                to_station_name = end_station_name,
                to_station_id = end_station_id, 
                usertype = member_casual)
# Step 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
colnames(all_dfs)
nrow(all_dfs)
dim(all_dfs)
head(all_dfs)
str(all_dfs)
summary(all_dfs)

# There are a few problems we will need to fix:
# (1) In the "usertype" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). 
# We will need to consolidate that from four to two labels.

all_dfs <- all_dfs %>% 
  mutate(usertype = recode(usertype,
                                "member" = "Subscriber",
                                "casual" = "Customer"))
table(all_dfs$usertype)
colnames(all_dfs)
all_dfs$date <- as.Date(all_dfs$start_time)
all_dfs$month <- format(as.Date(all_dfs$date), "%m")
all_dfs$day <- format(as.Date(all_dfs$date), "%d")
all_dfs$year <- format(as.Date(all_dfs$date), "%Y")
all_dfs$day_of_week <- format(as.Date(all_dfs$date), "%A")
# Add a "ride_length" calculation to all_dfs (in seconds)
all_dfs$ride_length <- difftime(all_dfs$end_time,all_dfs$start_time)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_dfs$ride_length)
all_dfs$ride_length <- as.numeric(as.character(all_dfs$ride_length))
is.numeric(all_dfs$ride_length)
# Removing "bad" data
all_trips <- all_dfs[!(all_dfs$from_station_name == "HQ QR" | all_dfs$ride_length<0),]
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_length)
median(all_trips$ride_length)
max(all_trips$ride_length)
min(all_trips$ride_length)
# OR
summary(all_trips$ride_length)
# Comparing Customers and Subscribers
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = min)
#  See the average ride time by each day for Subscribers vs Customer users
aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)
# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),			#calculates the number of rides and average duration 
  average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, weekday)	

# Let's visualize the number of rides by rider type
all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(),
  average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(),
  average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips$ride_length ~ all_trips$usertype + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/minba/OneDrive/Documents/R/projects/Bike_Share_project/avg_ride_length.csv')
