---
title: "Case-Study on Bellabeat"
author: "Taufeek Ahmed"
date: "2024-04-30"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<br>
<br>
<br>


## About The Company

Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company

<br>
<br>

## Buisness Goal

Identify potential opportunities for growth and recommendations for the Bellabeat marketing strategy improvement based on trends in smart device usage.

<br>
<br>

## Questions for the analysis
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

<br>
<br>

## Loading Packages
```{r echo=TRUE}
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
```
<br>
<br>

## Importing Dataset
 I am going to use Fitbit Fitness Tracker Dataset by [Kaggle](https://www.kaggle.com/datasets/arashnic/fitbit) 

```{r}
activity <- read.csv("dailyActivity_merged.csv")
calories <- read.csv("hourlyCalories_merged.csv")
intensities <- read.csv("hourlyIntensities_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
heart <- read.csv("heartrate_seconds_merged.csv")
```
<br>
While Exploring the data i found that date-time format is incorrect and inconsistant
<br>

### Formatting Date Time and Making new columns

```{r}
# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
# heart
heart$Time=as.POSIXct(heart$Time, format ="%m/%d/%Y %I:%M:%S %p", 
tz=Sys.timezone())
heart$date <- format(heart$Time, format="%m/%d/%y")
heart$Hour <- format(heart$Time , format ="%I")
```
### lets find out if data is enough for our analysis

```{r}
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)
n_distinct(heart$Id)
```

Note that the activity,calories,intensities dataset have 33 participants which is more relevant than weight and heart

### Lets find out the summary of this dataset
```{r}
# activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()
# heart
heart %>%
  select(Value) %>%
  summary()
```
### Some interesting discoveries from this summary:

- Average sedentary time is 991 minutes or 16 hours. Definately needs to be reduced!

- The majority of the participants are lightly active.

- On the average, participants sleep 1 time for 7 hours.

 Average total steps per day are 7638 which a little bit less for having health benefits for according to the CDC research. They found that taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps.

## Merging Data

Before beginning to visualize the data, I need to merge two data sets. I’m going to merge (inner join) activity and sleep on columns Id and date (that I previously created after converting data to date time format).

```{r}
merged_data <- merge(sleep, activity,by=c('Id', 'date'))
head(merged_data)
```

## Visualization

```{r}
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")
```
![here](https://drive.google.com/drive/u/0/folders/1CZeaMXz_NMVj1vWChUs9WwBmDhYg4s2m)
I see positive correlation here between Total Steps and Calories, which is obvious - the more active we are, the more calories we burn.

```{r}
ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")
```

The relationship between Total Minutes Asleep and Total Time in Bed looks linear. So if the Bellabeat users want to improve their sleep, we should consider using notification to go to sleep.

Let's look at intensities data over time (hourly).

```{r}
int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")
```

- After visualizing Total Intensity hourly, I found out that people are more active between 5 am and 10pm.

- Most activity happens between 5 pm and 7 pm - I suppose, that people go to a gym or for a walk after finishing work. We can use this time in the Bellabeat app to remind and motivate users to go for a run or walk.

Let's look at the relationship between Total Minutes Asleep and Sedentry Minutes.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r}
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")
```

- Here we can clearly see the negative relationship between Sedentary Minutes and Sleep time.

- As an idea: if Bellabeat users want to improve their sleep, Bellabeat app can recommend reducing sedentary time.

- Keep in mind that we need to support this insights with more data, because correlation between some data doesn’t mean causation.

```{r}
recordsVsSleep <- sleep %>%
  group_by(TotalSleepRecords) %>%
  drop_na() %>%
  summarise(Average_hour_sleep = mean(TotalMinutesAsleep)/60)

ggplot(data = recordsVsSleep, aes(y=Average_hour_sleep,x=TotalSleepRecords, color = "blue")) + geom_bar(position='dodge', stat='identity')+
  labs(title="TotalTimeInBed vs. TotalSleepRecords")
```

- People who sleeps 3 times a day have total sleep hour of 10+ which can correlate to increase in sedentary time

I had done more analysis on heart data but my R markdown keep crashing because i was using online R studio but i'll tell you what i found

- People who sleeps less than 5 hour have avg heartrate of 85-90 which is pretty high with respect to people who sleep more than 5+ hour which is 60-80

## Recommendations For The Business

- Average total steps per day are 7638 which a little bit less for having health benefits for according to the CDC research. They found that taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps. Bellabeat can encourage people to take at least 8 000 explaining the benefits for their health.

- If users want to lose weight, it’s probably a good idea to control daily calorie consumption. Bellabeat can suggest some ideas for low-calorie lunch and dinner.

- If users want to improve their sleep, Bellabeat should consider using app notifications to go to bed.

- Most activity happens between 5 pm and 7 pm - I suppose, that people go to a gym or for a walk after finishing work. Bellabeat can use this time to remind and motivate users to go for a run or walk.

- As an idea: if users want to improve their sleep, the Bellabeat app can recommend reducing sedentary time.

- As heart is high for those who sleeps less than 5 hours. so a pop-up notification to sleep to promote good heart health

