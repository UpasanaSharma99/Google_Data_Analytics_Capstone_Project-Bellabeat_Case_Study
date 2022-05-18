# Loading all necessary libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Reading CSV files into variables
daily_activity <- read.csv("C:/Richa/Google Data Analytics/Capstone Project/dailyActivity_merged.csv")
sleep_day <- read.csv("C:/Richa/Google Data Analytics/Capstone Project/sleepDay_merged.csv")
weightlog_info <- read.csv("C:/Richa/Google Data Analytics/Capstone Project/weightLogInfo_merged.csv")

##  Exploring tables

# Table - dailyActivity
head(daily_activity)
str(daily_activity)

# Table - sleepDay
head(sleep_day)
str(sleep_day)

# Table - weightInfo
head(weightlog_info)
str(weightlog_info)

# Understanding statistics summary of all three tables
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weightlog_info$Id)

## Creating a summary of the data
daily_activity_2<- read.csv("C:/Richa/Google Data Analytics/Capstone Project/dailyActivity_merged.csv")

# Changing the date format
daily_activity_2$ActivityDate = mdy(daily_activity_2$ActivityDate)

# Adding a new column 'day_name' for the corresponding dates 
daily_activity_1 <- daily_activity_2 %>%
  mutate(day_name = wday(daily_activity_2$ActivityDate, label = TRUE))

# Creating a new daily activity table summarized by days of the week
day_daily_activity <- daily_activity_1 %>%
  group_by(day_name) %>%
  summarise(total_steps = sum(TotalSteps), total_distance = sum(TotalDistance), total_calories = sum(Calories), Very_active_dist = sum(VeryActiveDistance),
            mod_active_dist = sum(ModeratelyActiveDistance), light_active_dist = sum(LightActiveDistance), sed_active_dist = sum(SedentaryActiveDistance), 
            very_active_min = sum(VeryActiveMinutes), fairly_active_min = sum(FairlyActiveMinutes), lightly_active_mins = sum(LightlyActiveMinutes),
            sed_min = sum(SedentaryMinutes)) %>%
  select(day_name, total_steps, total_distance, total_calories, Very_active_dist, mod_active_dist, light_active_dist, sed_active_dist, very_active_min,
         fairly_active_min, lightly_active_mins, sed_min)

# View the created table
print(day_daily_activity)

# Reading Sleepday_merged csv file
sleep_day_1 <- read.csv("C:/Richa/Google Data Analytics/Capstone Project/sleepDay_merged.csv")

# Changing the format of date
sleep_day_1$SleepDay = mdy_hms(sleep_day_1$SleepDay)

# Adding a new column 'day_name' for the corresponding dates
sleep_day_1 <- sleep_day_1 %>%
  mutate(day_name = wday(SleepDay, label = TRUE))

# Reading weightLogInfo_merged csv file
weightlog_info_1 <- read.csv("C:/Richa/Google Data Analytics/Capstone Project/weightLogInfo_merged.csv")

# Changing the format of date
weightlog_info_1$Date = mdy_hms(weightlog_info_1$Date)

# Adding a new column 'day_name' for the corresponding dates
weightlog_info_1 <- weightlog_info_1 %>%
  mutate(day_name = wday(Date, label = TRUE))

# Creating a new sleep_day table summarized by days of the week
day_sleep_day <- sleep_day_1 %>%
  group_by(day_name) %>%
  summarise(total_sleep_rec = sum(TotalSleepRecords), total_min_asleep = sum(TotalMinutesAsleep), total_time_bed = sum(TotalTimeInBed)) %>%
  select(day_name, total_sleep_rec, total_min_asleep, total_time_bed)

# View the created table
print(day_sleep_day)

# Combining day_daily_activity and day_sleep_day on 'day_name'
combined_activity_sleep <- left_join(day_daily_activity, day_sleep_day, 'day_name')

# View the combined table
print(combined_activity_sleep)

# Creating a summarized daily_activity table by Id
id_daily_activity <- daily_activity_1 %>%
  group_by(Id) %>%
  summarise(no_days = max(ActivityDate) - min(ActivityDate), total_steps = sum(TotalSteps), total_distance = sum(TotalDistance), tracker_dis = sum(TrackerDistance),
            log_activities_dist = sum(LoggedActivitiesDistance), Very_active_dist = sum(VeryActiveDistance), mod_active_dist = sum(ModeratelyActiveDistance), 
            light_active_dist = sum(LightActiveDistance), sed_active_dist = sum(SedentaryActiveDistance), very_active_min = sum(VeryActiveMinutes), 
            fairly_active_min = sum(FairlyActiveMinutes), lightly_active_mins = sum(LightlyActiveMinutes), sed_min = sum(SedentaryMinutes), 
            total_calories = sum(Calories)) %>% 
  select(Id, no_days, total_steps, total_distance, tracker_dis, log_activities_dist, Very_active_dist, mod_active_dist, light_active_dist, sed_active_dist, 
         very_active_min, fairly_active_min, lightly_active_mins, sed_min, total_calories) 

# Creating a summarized sleep_day table by Id
id_sleep_day <- sleep_day_1 %>%
  group_by(Id) %>%
  summarise(no_days = (max(SleepDay) - min(SleepDay)) / 86400,  total_sleep_rec = sum(TotalSleepRecords), total_min_asleep = sum(TotalMinutesAsleep), 
            total_time_bed = sum(TotalTimeInBed)) %>% 
  select(Id, no_days, total_sleep_rec, total_min_asleep, total_time_bed)

# Creating a summarized weightlog_info table by Id
id_weightlog_info <- weightlog_info_1 %>%
  group_by(Id) %>%
  summarise(no_days = (max(Date) - min(Date)) / 86400, max_weight = max(WeightKg), min_weight = min(WeightKg), max_fat = max(Fat), min_fat = min(Fat),
            max_bmi = max(BMI), min_bmi = min(BMI)) %>%
  select(Id, no_days, max_weight, min_weight, max_fat, min_fat, max_bmi, min_bmi)

# View tables created by Id
View(id_daily_activity)
View(id_sleep_day)
View(id_weightlog_info)

## Data Visualization

# total_steps Vs. day_of_week
ggplot(data = combined_activity_sleep, aes(x = day_name, y = total_steps, fill = day_name)) + geom_bar(stat = "identity")
# Observation: Calories has a positive correlation with no. of steps

# day_of_week Vs. mins_asleep
ggplot(data = combined_activity_sleep, aes(x = day_name, y = total_min_asleep , fill = day_name)) + geom_bar(stat = "identity")
# Observation: Total mins asleep also has a positive correlation with no. of steps

# total_calories Vs. day_of_week
ggplot(data = combined_activity_sleep, aes(x = day_name, y = total_calories, fill = day_name)) + geom_bar(stat = "identity")
# Observation: Calories also has a positive correlation with no. of steps

# total_steps Vs. sedentary_mins
ggplot(data = daily_activity) + geom_smooth(mapping = aes(x = TotalSteps, y = SedentaryMinutes))
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
# Observation: Sedentary minutes reduces as total steps increases, the least sedentary minutes are b/w 10,000 to 20,000 steps & spikes up after 20,000 steps

# total_mins_asleep Vs. total_time_in_bed
ggplot(data = sleep_day) + geom_smooth(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed))
## geom_smooth()` using method = 'loess' and formula 'y ~ x'
# Observation: Total time in bed increases with total mins asleep

