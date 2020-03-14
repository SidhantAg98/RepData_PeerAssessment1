---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data


```r
setwd("C:/Users/MAHE/Desktop/Coursera PE/Coursera Project/RepData_PeerAssessment1")
initialData <- read.csv("activity.csv")
```
An initial look at the data confirms its dimensions and contents

```r
head(initialData)
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
str(initialData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

Removing the missing values 

```r
data <- initialData[!(is.na(initialData$steps)), ]
```
calculating the total number of steps taken per day

```r
totalStepsDay <- aggregate(steps ~ date, data, sum)
```

histogram is created to indicate the frequency of total steps taken each day


```r
hist(totalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     main="Histogram of the Total Number of Steps Taken per Day",
     col=blues9)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

summarary function can calculate the mean and median values of the total number of steps taken per day


```r
summary(totalStepsDay$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```
Therefore the mean value calculated is **10766.19**, and the median value **10765**


## What is the average daily activity pattern?
Time plot should look at the average number of steps taken for each interval, the aggregate function is used. 


```r
meanStepsInterval <- aggregate(steps ~ interval, data, mean)
plot(x=meanStepsInterval$interval, y=meanStepsInterval$steps, type="l",
     main="Time Series Plot of Average Steps Taken per Interval",
     ylab="Number of Steps", xlab="Intervals (in 5 mins)",
     col="darkblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

 maximum number of steps
 
 ```r
 meanStepsInterval[grep(max(meanStepsInterval$steps), meanStepsInterval$steps), ]
 ```
 
 ```
 ##     interval    steps
 ## 104      835 206.1698
 ```
So the interval with the maximum number of steps is interval **835**.


## Imputing missing values
replace each missing value with the mean value for the same interval, averaged across all days.

```r
imputedData <- initialData
for(x in 1:17568) {
    if(is.na(imputedData[x, 1])==TRUE) {
        imputedData[x, 1] <- meanStepsInterval[meanStepsInterval$interval %in% imputedData[x, 3], 2]
    }
}
  
imputedTotalStepsDay <- aggregate(steps ~ date, imputedData, sum)
head(imputedTotalStepsDay)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```


```r
hist(imputedTotalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     main="Histogram of Total Number of Steps Taken per Day (With Imputed Values)",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

 mean and median total number of steps taken per day
 
 
 ```r
 imputedStepsSummary <- summary(imputedTotalStepsDay, 
                                 meanOfTotalSteps=mean(imputedTotalStepsDay$steps), 
                                 medianOfTotalSteps=median(imputedTotalStepsDay$steps))
                                 
 mean <- mean(imputedTotalStepsDay$steps)
 median <- median(imputedTotalStepsDay$steps)
 mean 
 ```
 
 ```
 ## [1] 10766.19
 ```
 
 ```r
 median
 ```
 
 ```
 ## [1] 10766.19
 ```
histograms of the two data sets (imputed and non-imputed) are compared:


```r
par(mfrow = c(1, 2))
hist(totalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     col="Blue", family="serif", ylim=c(0, 20), main=NULL)
hist(imputedTotalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     col="Red", family="serif", ylim=c(0, 20), main=NULL)
mtext("Histograms of Total Number of Steps Taken per Day, Without/With Imputed Values",
      adj=0.95, family="serif", font=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
length(unique(data$interval))
```

```
## [1] 288
```

missing observations are due to entirely missed days, (8 of the days) where no measurements were made whatsoever


## Are there differences in activity patterns between weekdays and weekends?
used the weekdays function to automatically calculate the day of the week

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.6.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
daysData <- imputedData
daysData$days <- weekdays(as.Date(daysData$date))
daysData$weekday <- as.character(rep(0, times=17568))
for(x in 1:17568) {
    if(daysData[x, 4] %in% c("Saturday", "Sunday")) {
        daysData[x, 5] <- "weekend"
    } else {
        daysData[x, 5] <- "weekday"
    }
}
daysData$weekday <- factor(daysData$weekday)
head(daysData)
```

```
##       steps       date interval   days weekday
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
## 6 2.0943396 2012-10-01       25 Monday weekday
```



```r
weekdayData <- daysData[daysData$weekday=="weekday", ]
weekendData <- daysData[daysData$weekday=="weekend", ]
```

average number of steps per interval is calculated

```r
weekdayMean <- aggregate(weekdayData$steps, list(weekdayData$interval), mean)
weekendMean <- aggregate(weekendData$steps, list(weekendData$interval), mean)
```

panel plot is created


```r
par(mfrow=c(2, 1), mar=c(4, 4.1, 3, 2.1))
plot(weekdayMean$Group.1,weekdayMean$x, type="l",
     main="Time Series Plot of Average Steps Taken per Interval, for Weekdays",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps",
     col="darkred", lwd=1.5, ylim=c(0, 230))
plot(weekendMean$Group.1, weekendMean$x, type="l",
     main="Time Series Plot of Average Steps Taken per Interval, for Weekends",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps", family="serif",
     col="darkblue", lwd=1.5, ylim=c(0, 230))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
