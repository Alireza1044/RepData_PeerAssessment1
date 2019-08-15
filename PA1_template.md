---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(lubridate)
library(knitr)
library(dplyr)
library(Hmisc)
library(ggplot2)
data <- read.csv("activity.csv",header = TRUE,sep = ",")
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day

```r
totalSteps <- data %>% group_by(day = date) %>% summarise(steps = sum(steps,na.rm = TRUE))
g <- ggplot(data = totalSteps,aes(day(day),steps))
g <- g + geom_histogram(stat = "identity") + xlab("Day")
g <- g + ylab("Steps") + ylim(c(0,40000)) + ggtitle(label = "Total steps/day")
g <- g + theme(plot.title = element_text(hjust = 0.5))
print(g)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

- Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps)
```

```
## [1] 9354.23
```

```r
median(totalSteps$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

- Make a time series plot (i.e. 
type = "l"
type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
Mean <- tapply(data[!is.na(data$steps),]$steps,data[!is.na(data$steps),]$interval,mean)
plot(names(Mean),Mean,ylab ="Average Steps",xlab="Intervals",type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Mean[which.max(Mean)]
```

```
##      835 
## 206.1698
```

## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NA
NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_impute <- data
Mean <- tapply(data[!is.na(data$steps),]$steps,data[!is.na(data$steps),]$interval,mean)
data_impute[(data_impute$interval == names(Mean) & is.na(data_impute$steps)),]$steps <- Mean
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalSteps <- data_impute %>% group_by(day = date) %>% summarise(steps = sum(steps,na.rm = TRUE))
g <- ggplot(data = totalSteps,aes(day(day),steps))
g <- g + geom_histogram(stat = "identity") + xlab("Day")
g <- g + ylab("Steps") + ylim(c(0,40000)) + ggtitle(label = "Total steps/day")
g <- g + theme(plot.title = element_text(hjust = 0.5))
print(g)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_impute$datetype <- sapply(data_impute$date,function(x)
{ 
    if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
    {
        y <- "Weekend"
    }
    else
    {
        y <- "Weekday"
    }
    y
})
```

- Make a panel plot containing a time series plot (i.e. 
type = "l"
type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Mean <- aggregate(steps ~ datetype+interval, data=data_impute, FUN=mean)
g <- ggplot(Mean,aes(interval,steps)) + geom_line() + facet_grid(datetype~.)
g <- g + xlab(label = "Interval") + ylab(label = "Average Steps")
g <- g + ggtitle(label = "Average daily steps by datetype")
g <- g + theme(plot.title = element_text(hjust = 0.5))
print(g)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />
