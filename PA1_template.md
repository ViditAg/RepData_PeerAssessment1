---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
Activity_data<-read.csv("activity/activity.csv",header=TRUE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
options(warn=-1)
```

## What is mean total number of steps taken per day?

```r
Datewise_sum<-summarise_all(group_by(Activity_data,date),sum,na.rm=TRUE)
```

```r
hist(Datewise_sum$steps,xlab='Total steps taken each day',main='Histogram of Total steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Total number of steps taken per day: Mean

```r
mean(Datewise_sum$steps)
```

```
## [1] 9354.23
```

Total number of steps taken per day: Median

```r
median(Datewise_sum$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
Interval_mean<-summarise_all(group_by(Activity_data,interval),mean,na.rm=TRUE)
with(Interval_mean,plot(interval,steps,type="l",xlab="Interval",main="Steps in 5-minute averaged over all dates"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

5-minute interval with max. number of steps averaged over days

```r
Interval_mean$interval[which(Interval_mean$steps==max(Interval_mean$steps))]
```

```
## [1] 835
```

## Imputing missing values

Total number of missing (NA) values.

```r
sum(is.na(Activity_data$steps))
```

```
## [1] 2304
```

### Fixed missing values by imputing mean value from 5-minute interval.

All results are calculated from data after filling the missing values


```r
Activity_data_new<-Activity_data
for (interval in Interval_mean$interval){
    Interval_imask<-Activity_data_new$interval==interval
    mean_val<-mean(Activity_data_new$steps[Interval_imask],na.rm=TRUE)
    Activity_data_new$steps<-replace(Activity_data_new$steps,Interval_imask & is.na(Activity_data_new$steps),mean_val)  
}
Datewise_sum_new<-summarise_all(group_by(Activity_data_new,date),sum,na.rm=TRUE)
```



```r
hist(Datewise_sum_new$steps,xlab='Total steps taken each day',main='Histogram of Total steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Total number of steps taken per day: Mean

```r
mean(Datewise_sum_new$steps)
```

```
## [1] 10766.19
```

Total number of steps taken per day: Median

```r
median(Datewise_sum_new$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
WeekDays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
Activity_data_new<-mutate(Activity_data_new,Day_label=factor(weekdays(as.Date(Activity_data_new$date))%in%WeekDays,labels=c('Weekday','Weekend')))
```

Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
Weekend_activity<-Activity_data_new[Activity_data_new$Day_label=='Weekend',]
Weekday_activity<-Activity_data_new[Activity_data_new$Day_label=='Weekday',]
Weekend_Interval_mean<-summarise_all(group_by(Weekend_activity,interval),mean,na.rm=TRUE)
Weekday_Interval_mean<-summarise_all(group_by(Weekday_activity,interval),mean,na.rm=TRUE)
par(mfrow=c(2,1))
par(mar=c(4,4,1,2))
with(Weekend_Interval_mean,plot(interval,steps,type="l",main='weekend',xaxt='n',ylab='',xlab=''))
par(mar=c(4,4,1,2))
with(Weekday_Interval_mean,plot(interval,steps,type="l",main='weekday',ylab='',xlab='Interval'))
mtext('Number of steps',line=-2,side=2,outer=TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
