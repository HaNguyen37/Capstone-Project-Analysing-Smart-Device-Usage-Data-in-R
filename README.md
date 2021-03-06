# Google Data Analytics Professional Capstone Project: Analysing Smart Device Usage Data in R

## Overview

This Case Study is a Capstone project for the Google Data Analytics Course by Coursera. For the purpose of this case study, I am a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused products for women. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.

The goal of this project is to analyse smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices. Then, using this information to provide the high-level recommendations for how these trends can inform Bellabeat marketing strategy.

## Prepare data for exploration

The dataset being used is public data that explores smart device users' daily habits: FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius). This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits. 

The dataset for analysis is stored locally for the purpose of this project. The data is original, comprehensive and cited. However, it includes a small sample, a small period of data collection (between 03.12.2016-05.12.2016.) and it doesn’t show any demographic information, which translates in highly possibly biased data. Another limitation is that the data is not current (2016).

## Processing Data

Tools used: **R**

### Installing and loading common packages and libraries


```{r}
install.packages('tidyverse')
install.packages("tidyr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("dplyr")
install.packages("lubridate")
library(tidyr)
library(ggplot2)
library(janitor)
library(dplyr)
library(lubridate)
library(tidyverse)
```


### Loading your CSV files

Here we'll create a dataframe named 'daily_activity' and read in one of the CSV files from the dataset.

```{r}
daily_activity <- read.csv("dailyActivity_merged.csv")
```

We'll create another dataframe for the sleep data. 

```{r}
sleep_day <- read.csv("sleepDay_merged.csv")
minute_sleep <- read.csv("minuteSleep_merged.csv")
```

We'll create another dataframe for the step and calories data.

```{r}
daily_steps <- read.csv("dailySteps_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")
```


### Exploring a few key tables

- Take a look at the daily_activity data.

```{r}
head(daily_activity)
```

- Identify all the columsn in the daily_activity data.

```{r}
colnames(daily_activity)
```

- Take a look at the sleep_day data.

```{r}
head(sleep_day)
```

- Identify all the columsn in the sleep_day data.

```{r}
colnames(sleep_day)
```

- Take a look at the minute_sleep data.

```{r}
head(minute_sleep)
```

- Identify all the columsn in the minute_sleep data.

```{r}
colnames(minute_sleep)
```

- Take a look at the daily_steeps data

```{r}
head(daily_steps)
```

- Identify all the columsn in the daily_steps data.

```{r}
colnames(daily_steps)
```

- Take a look at the daily_calories data

```{r}
head(daily_calories)
```

- Identify all the columsn in the daily_calories data

```{r}
colnames(daily_calories)
```

**Note that both datasets have the 'Id' field - this can be used to merge the datasets.**

We notice that the daily_activity dataframe already includes data in the daily_calories and daily_steps dataframe. Thus, we remove these two dataframes.

```{r}
rm(daily_calories,daily_steps)
```

### Structure of the dataframes

- First, we take a look ar the dataframes daily_activity and sleep_day

```{r}
glimpse(daily_activity)
glimpse(sleep_day)
```

- Check for duplicates

```{r}
sum(duplicated(daily_activity)) 
sum(duplicated(sleep_day))
```

- Remove duplicate values in sleep_day data

```{r}
sleep_day <- sleep_day %>%
  distinct()
```

- Remove rows with empty fields

```{r}
hourly_steps <- hourly_steps %>%
  drop_na(ActivityHour)

hourly_calories <- hourly_calories %>%
  drop_na(ActivityHour)
```

- Correct format of the date columns

```{r}
daily_activity <- daily_activity %>%
  mutate(ActivityDate = as_date(ActivityDate, format = "%m/%d/%Y"))
```
```{r}
sleep_day <- sleep_day %>%
  mutate(SleepDay = as_date(SleepDay , format = "%m/%d/%Y"))
```

```{r}
hourly_steps$date <- as_date(mdy_hms(hourly_steps$ActivityHour))
hourly_steps$time<- format(as.POSIXct(mdy_hms(hourly_steps$ActivityHour)), format = "%H:%M")
hourly_steps$time <- hour(mdy_hms(hourly_steps$ActivityHour)) #%>% hour(hourly_steps$ActivityHour)
hourly_steps$day<- weekdays(hourly_steps$date)
head(hourly_steps)
```

```{r}
hourly_calories$date <- as_date(mdy_hms(hourly_calories$ActivityHour))
hourly_calories$time <- format(as.POSIXct(mdy_hms(hourly_calories$ActivityHour)),format = "%H:%M")
hourly_calories$time <- hour(mdy_hms(hourly_calories$ActivityHour))
hourly_calories$day <- weekdays(hourly_calories$date)
head(hourly_calories)
```

- Fixing format

```{r}
d <- unique(daily_activity$ActivityDate)
print(d)

class(daily_activity$ActivityDate)

head(daily_activity)
```

- Check the unique participants are there in each dataframe

```{r distinct users}
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
```

- Check number of observationn in each dataframe

```{r observations}
nrow(daily_activity)
nrow(sleep_day)
```


### Merging these two datasets together

- Rename the columns with date to a same name

```{r}
daily_activity <-  daily_activity %>%
  rename(date = ActivityDate) 

sleep_day <-  sleep_day %>%
  rename(date = SleepDay)
```


_**Note**: There were more participant Ids in the daily_activity dataset than in sleep_day dataset that lead to some Ids in daily_activity have been filtered out using merge_.

```{r}
combined_data <- merge(sleep_day, daily_activity, by=c("Id","date"))
```

```{r}
glimpse(combined_data)
```

- Take a look at how many participants are in this data set

```{r}
n_distinct(combined_data$Id)
```

- Add weekday column to comnbined_data

```{r}
final_daily <- combined_data
final_daily$weekday <- weekdays(final_daily$date)
glimpse(final_daily)
```
## Analyse and Share Phases

### Summary statistics

- **For the daily activity dataset:**

```{r}
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()
```

- **For the sleep dataset:**

```{r}
sleep_day %>%  
  select(TotalSleepRecords,
  TotalMinutesAsleep,
  TotalTimeInBed) %>%
  summary()
```
```{r}
summary(final_daily)
```

- **The average:**

```{r}
daily_average <- combined_data %>%
  group_by(Id) %>%
  summarise(average_steps = mean(TotalSteps), average_calories = mean(Calories), average_minutes_sleep = mean(TotalMinutesAsleep), average_time_in_bed = mean(TotalTimeInBed))
head(daily_average)
```
```{r}
summary(daily_average)
```
**Note**: 
- The average calories burn for this sample = 2397 while the maximum burned calories = 3539
- The average steps count for this sample = 7880 while the maximum steps count = 19079

### Plotting a few explorations

- **Correlations between Total Steps and Sedentary Minutes**

```{r}
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()
```

- **Correlations between Total time in bed and Total Minutes Asleep**

```{r}
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()
```

**Insight**: The correlation between minutes asleep and time in bed is almost linear

- **Correlations between Steps and Calories**

```{r}
ggplot(final_daily, aes(x=TotalSteps, y=Calories))+
  geom_jitter()+
  geom_smooth(color = "turquoise")+
  labs(title = "Steps vs Calories", x = "Steps", y = "Calories")+
  theme(panel.background = element_blank(), plot.title = element_text( size=22))
```

**Insight**: There is a positive correlation between the total number of steps and the burned calories

### Activity levels

- Use a guideline on steps and activity levels as the classification levels:
  Sedentary is less than 5,000 steps per day 
  Low active is 5,000 to 7,499 steps per day
  Somewhat active is 7,500 to 9,999 steps per day
  Active is more than 10,000 steps per day
  Highly active is more than 12,500
  
```{r}
classification_steps_day <- tibble(
  steps_day = c('<5000', '5000 - 7499', '7500 - 9999', '>10000'),
  activity_level = c('Sedentary', 'Low active', 'Somewhat active', 'Active')
)

print(classification_steps_day)
```

- Assign this classification to the data

```{r}
daily_average_levels <- daily_average %>%
  mutate(activity_level = case_when(
    average_steps < 5000 ~ 'Sedentary',
    average_steps >= 5000 & average_steps < 7500 ~ 'Low active',
    average_steps >= 7500 & average_steps < 10000 ~ 'Somewhat active',
    average_steps >= 10000 ~ 'Active'
    ))
```

- Calculate the percentage of users for each activity level

```{r}
activity_level_percentage <- daily_average_levels %>%
  group_by(activity_level) %>%
  summarise(total_level=n()) %>%
  mutate(percentage = (total_level /sum(total_level))) %>%
  mutate(percentage = formattable::percent(percentage)) %>%
  arrange((activity_level))
```

```{r}
glimpse(activity_level_percentage)
```

- Create a visualization for activity levels

```{r}
ggplot(activity_level_percentage, aes(x="", y=percentage, fill=activity_level))+
  geom_bar(width=1, stat="identity", color="white")+
  coord_polar("y", start = 0)+
  geom_text(aes(label=percentage), position = position_stack(vjust = 0.45), size = 5)+
  labs(title = "Activity level distribution")+
  scale_fill_brewer(palette ="BrBG")+
  guides(fill = guide_legend(title=NULL))+
  theme_void()+
theme(plot.title = element_text(size=22), legend.text = element_text(size=15))
```

**Insight**: The biggest part of the users (37.50%) is somewhat active, with an average between 7500 and 9999 steps per day, meanwhile there are as many active users as low active and sedentary.

- **Calculate average total steps by day of week**

```{r}
final_daily$weekday <- factor(final_daily$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
final_daily %>% group_by(weekday) %>% summarise(Mean_total_steps= round(mean(TotalSteps, na.rm = TRUE), 0)) %>% 
  ggplot(aes(weekday, Mean_total_steps , fill= weekday ))+
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip() +
  geom_text(aes(label= Mean_total_steps ), hjust=1.1, vjust=.5, color="black",position = position_dodge(), size=6 )+
  #scale_fill_viridis_d() +
  labs(title = "Total of Steps by Day of Week", x="Day of Week", y="Total Steps") +
  theme(plot.subtitle = element_text(color = "black" , face = "italic"), legend.position = "non" )

```

**Insight**: Saturday has the most active day by users.

- **Calculate daily active time by hour**

- Merge hourly_steps and hourly_calories

```{r}
n_distinct(hourly_steps)
n_distinct(hourly_calories)
combined_hourly <- merge(hourly_calories, hourly_steps)
```

```{r}
glimpse(combined_hourly)
```
- Visualization

```{r}
combined_hourly$day<- factor(combined_hourly$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
n_distinct(combined_hourly)

ggplot(combined_hourly, aes(time, day, fill= StepTotal)) +  geom_tile(color= "grey", lwd=.4 , linetype =1) + coord_fixed() + scale_fill_gradient(low= "white", high="red2") + theme(axis.text.x= element_text(angle = 90)) + labs(title = " Total Steps by Hour")+ theme(panel.background = element_rect(), legend.position = "right")
```

**Insight**: Most of participants activity hours during the week days between 9:00AM and 4:00PM

## Recommendations

Marketing Campaigns are recommended to be conducted on Saturday and during the daytime to attract more active users.

Most of the users belong to Somewhat active, with average steps between 7500 and 9999. Thus, Bellabeat smart devices with notification functions of total steps reminder and provide tips about how to gain more steps might encourage users to exceed 10000 steps per day.

Other functions such as bed-time reminders and total daily calories calculating would be necessary for users.

For further analysis, I would recommend Bellabeat store a bigger sample of data and include characteristics such as age, demographics, preferences, and lifestyle. The data could be obtained from periodic surveys done through the Bellabeat app.

--

Thank you!
