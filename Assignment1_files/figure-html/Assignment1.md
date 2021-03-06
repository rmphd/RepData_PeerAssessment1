---
title: "Reproducible Research Assignment 1"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---
**Load all the necessary packages:**

```r
library(knitr)
opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set( fig.path = "figs/fig-")
library(ggplot2)
```
**Load the data and set class**

```r
setwd("~/Documents/datasciencecoursera/Reproducable Research/Course Project 1")
activity = read.csv("activity.csv", colClasses = c('numeric', 'Date', 'integer'))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
**_What is mean total number of steps taken per day?_**

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
#1. Calculate the average number of steps, per day (ignoring missing data)
activity_by_day = aggregate(steps ~ date,activity,sum,rm.na = TRUE)
#2. Plot the mean total number of steps using a historgram
plot1 = ggplot(data = activity_by_day, aes(date, steps))
plot1 = plot1 + geom_bar(stat="identity") + scale_x_date(breaks = "2 weeks") 
plot1 = plot1 + labs(title = "Total Number of Steps per Day", y = "Total Number of Steps")
plot1 = plot1 + xlab ("Date")
print(plot1)
```

![](figs/fig-mean_steps_per_day-1.png)<!-- -->

```r
#3. Calculate mean and median total number of steps taken per day
mean_steps = as.integer(mean(activity_by_day$steps))
print(mean_steps)
```

```
## [1] 10767
```

```r
median_steps = as.integer(median(activity_by_day$steps))
print(median_steps)
```

```
## [1] 10766
```
**_What is the average daily activity pattern?_**
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#1a. Calculate the mean number of steps per interval (across all days, ignoring missing data)
activity_by_interval = aggregate(steps ~ interval, activity, mean, rm.na = TRUE)
#1b. Create a time series plot of 5-minute intervals and mean number of steps taken for that interval averaged across all days
plot2 = plot(activity_by_interval$interval, activity_by_interval$steps, type ="l", main = "Average Steps by Interval", xlab = "5-min Interval", ylab = "Ave Steps")
```

![](figs/fig-daily_activity_pattern-1.png)<!-- -->

```r
#2. Which 5-min interval, on ave, contains the max number of steps?
max_interval = activity_by_interval[which.max(activity_by_interval$steps),]
print(as.integer(max_interval))
```

```
## [1] 835 206
```
**_Imputing Missing Values_**
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. 
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

_Do these values differ from the estimates from the first part of the assignment?_
_What is the impact of imputing missing data on the estimates of the total daily number of steps?_

```r
#1. Calculate the total number of missing values in the dataset
NA_count = sum(is.na(activity$steps))
#2. Strategy for Imputing: For all rows with NA impute and replace the value using the ave steps for that interval across all other days
#3a. Create new dataset
imp_Data = activity 
for (i in 1:nrow(imp_Data)) {
        if (is.na(imp_Data$steps[i])) {
                imp_Data$steps[i] = activity_by_interval[which(imp_Data$interval[i] == activity_by_interval$interval), ]$steps
        }
}
#3b. Confirm that all missing values have been replaced by counting the number of observations that now have NA in the steps factor.
print(sum(!complete.cases(imp_Data)))
```

```
## [1] 0
```

```r
#4a. Make a historgram of the full new data set, including imputed values, showing the total steps per day.
plot3 = ggplot(data = imp_Data, aes(date, steps)) 
plot3 = plot3 + geom_bar(stat="identity") #stat = identity gives the value in stead of hte number of observances with that value
plot3 = plot3 + scale_x_date(breaks = "2 weeks") #setting xaxis buckets
plot3 = plot3 + labs(title = "Total Number of Steps per Day with Imputed Data", y = "Total Number of Steps")
plot3 = plot3 + xlab ("Date")
print(plot3)
```

![](figs/fig-imputing-1.png)<!-- -->

```r
#4b. Calculate the mean total number of steps taken per day with imputed data
activity_by_day_Imputed = aggregate(steps ~ date,imp_Data,sum)
#Calculate the mean and median total number of steps taken per day with imputed data
mean_steps_Imputed = as.integer(mean(activity_by_day_Imputed$steps))
median_steps_Imputed = as.integer(median(activity_by_day_Imputed$steps))
print(mean_steps_Imputed)
```

```
## [1] 10766
```

```r
print(median_steps_Imputed)
```

```
## [1] 10766
```
Overall the mean and median calculated from the original dataset with missing values, and the mean and median resulting from imputing the missing values are very similar. However, the mean is a little lower than the original dataset, ignoring NAs. The median is the same as observed in the original dataset.

**_Are there differences in activity patterns between weekdays and weekends?_**
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
#1. Create a new weekday variable to determine if the date of the observation is a weekday or weekend.
#determine what day of the week the observation date was
imp_Data$DayofWeek = as.factor(weekdays(imp_Data$date)) 
#determine whether a weekday or weekend based on the day of th week
imp_Data$dayType = as.factor(weekdays(imp_Data$date)) 
levels(imp_Data$dayType) = list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
#2. Create a time series plot of 5-minute intervals and average steps taken averaged across - panel plot of weekends vs. weekdays
interval_dayType = aggregate(data = imp_Data, steps ~ dayType + interval, mean)
plot4 = ggplot(interval_dayType, aes(x = interval, y = steps, color = dayType))
plot4 = plot4 + geom_line() + facet_wrap(~dayType, ncol = 1 , nrow = 2)
plot4 = plot4 + labs(title = "Time Series of Activity by Day Type", x = "5-min Interval", y = "Ave. Steps")
print(plot4)
```

![](figs/fig-weekdays-1.png)<!-- -->

The pattern looks largely the same for activity for weekdays and weekends.
