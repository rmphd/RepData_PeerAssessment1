#Load the data
activity = read.csv("activity.csv", colClasses = c('integer', 'Date', 'factor'))
#mean total number of steps taken per day
activity_by_day = aggregate(steps ~ date,activity,sum,rm.na = TRUE)
#plot the mean total number of stesps using a historgram
library(ggplot2)
plot1 = ggplot(data = activity_by_day, aes(date, steps)) + geom_bar(stat="identity") + scale_x_date(breaks = "2 weeks") 
plot1 = plot1 + labs(title = "Total Number of Steps per Day", y = "Total Number of Steps")
plot1 = plot1 + xlab ("Date")
print(plot1)
#mean and median total number of steps taken per day
mean_steps = as.integer(mean(activity_by_day$steps))
median_steps = as.integer(median(activity_by_day$steps))

#time series plot of 5-minute intervals and average steps taken averaged across all days
activity_by_interval = aggregate(steps ~ interval, activity, mean, rm.na = TRUE)
plot2 = plot(activity_by_interval$interval, activity_by_interval$steps, type ="l", main = "Average Steps by Interval", xlab = "5-min Interval", ylab = "Ave Steps")
#which 5-min interval, on ave, contains the max number of steps
max_interval = activity_by_interval[which.max(activity_by_interval$steps),]

#Calculate the total number of missing values in the dataset
NA_count = sum(is.na(activity$steps))
#for all rows with NA impute and replace the value using the ave steps for that interval across
#all other days
imp_Data = activity
for (i in 1:nrow(imp_Data)) {
        if (is.na(imp_Data$steps[i])) {
                imp_Data$steps[i] = activity_by_interval[which(imp_Data$interval[i] == activity_by_interval$interval), ]$steps
        }
}
#how many observations now have NA in the steps factor?
sum(!complete.cases(imp_Data))
#make a historgram of the full data set for total steps per day, including imputed data
plot3 = ggplot(data = imp_Data, aes(date, steps)) + geom_bar(stat="identity") + scale_x_date(breaks = "2 weeks") 
plot3 = plot3 + labs(title = "Total Number of Steps per Day with Imputed Data", y = "Total Number of Steps")
plot3 = plot3 + xlab ("Date")
print(plot3)
#mean total number of steps taken per day with imputed data
activity_by_day_Imputed = aggregate(steps ~ date,imp_Data,sum)
#mean and median total number of steps taken per day with imputed data
mean_steps_Imputed = as.integer(mean(activity_by_day_Imputed$steps))
median_steps_Imputed = as.integer(median(activity_by_day_Imputed$steps))
#Do these values differ from the estimates from the first part of the assignment?
        #The mean is 1 step lower than the original dataset, ignoring NAs
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
        #The median is the same as observed in the original dataset

#look for differences in activity patterns between weekdays and weekends
imp_Data$DayofWeek = as.factor(weekdays(imp_Data$date))
imp_Data$dayType = as.factor(weekdays(imp_Data$date))
levels(imp_Data$dayType) = list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
#time series plot of 5-minute intervals and average steps taken averaged across - panel plot of weekends vs. weekdays
interval_dayType = aggregate(data = imp_Data, steps ~ dayType + interval, mean)
plot4 = plot(interval_dayType$interval, interval_dayType$steps, type ="l", main = "Average Steps by Interval", xlab = "5-min Interval", ylab = "Ave Steps")


plot4 = ggplot(interval_dayType, aes(x = interval, y = steps, color = dayType))
plot4 = plot4 + geom_line() + facet_wrap(~dayType, ncol = 1 , nrow = 2)
plot4 = plot4 + labs(title = "Time Series of Activity by Day Type", x = "5-min Interval", y = "Ave. Steps")
print(plot4)

