# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
Set the working directory and load packages needed

```r
setwd("C:/_almacen/coursera/reproducible research/course project 1")
library(dplyr)
library(ggplot2)
library(lattice)
library(knitr)
```
##### 1. Load the data:

```r
raw_data<- read.csv("activity.csv", na.strings= "NA", header= TRUE)
```
##### 2. Transform "date" variable to Date format:

```r
raw_data$date= as.Date(as.character(raw_data$date), "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day (dplyr library was used)

```r
steps_per_day<- raw_data %>%
        group_by(date) %>% 
        summarize(total= sum(steps, na.rm= TRUE))
```
##### 2. Make a histogram of the total number of steps taken each day (ggplot2 library was used)

```r
p1<- ggplot(steps_per_day, aes(total))+ geom_histogram(fill= "grey60")
p1<- p1+ xlab("Total number of steps taken each day")+ ylab("Frequency")
print(p1)
```
##### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
print(paste("The mean of the total number of steps taken each day is: ", round(mean(steps_per_day$total), 2)))
```

```
## [1] "The mean of the total number of steps taken each day is:  9354.23"
```

```r
print(paste("The median of the total number of steps taken each day is: ", median(steps_per_day$total)))
```

```
## [1] "The median of the total number of steps taken each day is:  10395"
```
## What is the average daily activity pattern?
##### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) (ggplot2 library was used)

```r
# Calculate the average number of steps taken by interval
interval_average<- raw_data %>%
        group_by(interval) %>% 
        summarize(ave= mean(steps, na.rm= TRUE))

p2<- ggplot(interval_average, aes(interval, ave)) + geom_line() 
p2<- p2+ ggtitle("Daily average steps by interval")+ xlab("Interval")+ ylab("Average steps taken")
print(p2)
```
##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval<- which.max(interval_average$ave)
print(paste("The 5-minute interval that contains the maximum number of steps is ", interval_average[max_interval, 1]))
```

```
## [1] "The 5-minute interval that contains the maximum number of steps is  835"
```
## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset

```r
print(paste("There are", sum(is.na(raw_data)), "missing values in the dataset"))
```

```
## [1] "There are 2304 missing values in the dataset"
```
##### 2. Devise a strategy for filling in all of the missing values in the dataset.
To impute the data, the 5-minute interval mean for all days will be used.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in (dplyr library was used).

```r
imputed_data<- raw_data %>% 
        left_join(interval_average) %>%
        mutate(steps= ifelse(is.na(steps), ave, steps))
```
##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? (ggplot2 library was used)

```r
p3<- ggplot(imputed_data, aes(steps))+ geom_histogram(fill= "grey60")
p3<- p3+ xlab("Total number of steps taken each day with imputed data")+ ylab("Frequency")
print(p3)
```

```r
print(paste('The mean of the total number of steps taken each day is:', round(mean(imputed_data$steps), 2)))
```

```
## [1] "The mean of the total number of steps taken each day is: 37.38"
```

```r
print(paste('The median of the total number of steps taken each day is:', median(imputed_data$steps)))
```

```
## [1] "The median of the total number of steps taken each day is: 0"
```
The impact of imputing missing data is very high, in both estimates, mean and median, with a variation from 9354.23 to 37.38 in the mean, and a variation from 10395 to 0 in the median. This is caused because the imputed data is 0 in almost all the cases.

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day (ggplot2 library was used)

```r
imputed_data<- imputed_data %>%
        mutate(date= as.Date(as.character(date, "%Y-%m-%d"))) %>%
        mutate(weekday= weekdays(date)) %>%
        mutate(week_factor= ifelse(weekday== "sabado"| weekday== "domingo", "weekend", "weekday")) %>%
        group_by(interval, week_factor) %>% 
        summarize(ave_f= mean(steps)) 
```
##### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) (lattice library was used)

```r
xyplot(ave_f~ interval| week_factor, data=imputed_data, layout= c(1, 2), type= "l", ylab= "Number of steps")
```
