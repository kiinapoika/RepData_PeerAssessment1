# Reproducible Research: Peer Assessment 1

## Overview
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

###Load packages


```r
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "data.table", "lubridate")

ipak(packages)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: data.table
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```
##    ggplot2 data.table  lubridate 
##       TRUE       TRUE       TRUE
```

###Load the data

Set file directories

```r
file.dir<-"C:/path/"
```

Download & unzip data

```r
tmp <- paste0(file.dir, "data.zip")
if(!file.exists(tmp)) {
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, tmp)
unzip(tmp)
}
```

Read in the dataset

```r
data<-fread(paste0(file.dir, "activity.csv"),
            header = TRUE,
            sep = ",",
            dec = ".")
```

Inspect the first 10 rows

```r
head(data, 10)
```

```
##     steps       date interval
##  1:    NA 2012-10-01        0
##  2:    NA 2012-10-01        5
##  3:    NA 2012-10-01       10
##  4:    NA 2012-10-01       15
##  5:    NA 2012-10-01       20
##  6:    NA 2012-10-01       25
##  7:    NA 2012-10-01       30
##  8:    NA 2012-10-01       35
##  9:    NA 2012-10-01       40
## 10:    NA 2012-10-01       45
```

Inspect the data structure

```r
str(data)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

Process/transform the data
The variable "date" has the character class, so it needs to be transformed into date format


```r
data$date<-as.Date(data$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day? 
For this part of the assignment, you can ignore the missing values in the dataset.

###1. Calculate the total number of steps taken per day
Aggregate steps by day

```r
steps.by.day<-data[,list(total.steps=sum(steps, na.rm = TRUE)),
                   by=list(date)]

steps.by.day<-steps.by.day[complete.cases(steps.by.day)]

head(steps.by.day, 10)
```

```
##           date total.steps
##  1: 2012-10-01           0
##  2: 2012-10-02         126
##  3: 2012-10-03       11352
##  4: 2012-10-04       12116
##  5: 2012-10-05       13294
##  6: 2012-10-06       15420
##  7: 2012-10-07       11015
##  8: 2012-10-08           0
##  9: 2012-10-09       12811
## 10: 2012-10-10        9900
```

###2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
ggplot(steps.by.day, aes(x=total.steps))+
    geom_histogram(fill = "dark grey", binwidth = 1000)+
    labs(x="Total steps per day", y="Frequency", title="Distribution of total steps\n taken per day")+
    theme(plot.title = element_text(hjust = 0.5))
```

![plot1](https://github.com/kiinapoika/RepData_PeerAssessment1/blob/master/plot1.png)<!-- -->

###3. Calculate and report the mean and median of the total number of steps taken per day
mean of the total number of steps taken per day

```r
mean(steps.by.day$total.steps)
```

```
## [1] 9354.23
```

median of the total number of steps taken per day

```r
median(steps.by.day$total.steps)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Aggregate steps per interval

```r
steps.by.interval<-data[,list(avg.steps=mean(steps, na.rm = TRUE)),
                   by=list(interval)]
```

Time series plot

```r
ggplot(steps.by.interval, aes(x=interval, y=avg.steps))+
    geom_line()+
    labs(x="5-minute interval", y="Average steps taken per interval", title="Time series of average steps\n taken per 5-minute interval")+
    theme(plot.title = element_text(hjust = 0.5))
```

![plot2](https://github.com/kiinapoika/RepData_PeerAssessment1/blob/master/plot2.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.by.interval[order(-steps.by.interval$avg.steps),]$interval[[1]]
```

```
## [1] 835
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(data[!complete.cases(data), ])
```

```
## [1] 2304
```

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Calculate the mean for each 5-minute interval

```r
data[, interval.mean:=mean(steps, na.rm = TRUE), by=interval]
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Impute missing values

```r
data.imputed<-data[, list(steps=ifelse(is.na(steps), interval.mean, steps),
                          date=ymd(date),
                          interval=interval)]
head(data.imputed, 10)
```

```
##         steps       date interval
##  1: 1.7169811 2012-10-01        0
##  2: 0.3396226 2012-10-01        5
##  3: 0.1320755 2012-10-01       10
##  4: 0.1509434 2012-10-01       15
##  5: 0.0754717 2012-10-01       20
##  6: 2.0943396 2012-10-01       25
##  7: 0.5283019 2012-10-01       30
##  8: 0.8679245 2012-10-01       35
##  9: 0.0000000 2012-10-01       40
## 10: 1.4716981 2012-10-01       45
```

Inspect new dataset

```r
str(data.imputed)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

Check for missing values

```r
nrow(data.imputed[!complete.cases(data.imputed), ])
```

```
## [1] 0
```
The new dataset has no more missing values.

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Aggregate steps by day

```r
steps.by.day<-data.imputed[,list(total.steps=sum(steps, na.rm = TRUE)),
                   by=list(date)]
```

Histogram

```r
ggplot(steps.by.day, aes(x=total.steps))+
    geom_histogram(fill = "dark grey", binwidth = 1000)+
    labs(x="Total steps per day", y="Frequency", title="Distribution of total steps\n taken per day (No missings)")+
    theme(plot.title = element_text(hjust = 0.5))
```

![plot3](https://github.com/kiinapoika/RepData_PeerAssessment1/blob/master/plot3.png)<!-- -->

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data.imputed$weekday<-as.factor(ifelse(weekdays(data.imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
```

###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Aggregate steps by interval

```r
steps.by.interval<-data.imputed[,list(avg.steps=mean(steps)),
                   by=list(interval, weekday)]
```

Plot

```r
ggplot(steps.by.interval, aes(x=interval, y=avg.steps))+
    geom_line()+
    labs(x="5-minute interval", y="Average steps taken per interval", title="Time series of average steps\n taken per 5-minute interval")+
    facet_wrap(~weekday, ncol=1)+
    theme(plot.title = element_text(hjust = 0.5))
```

![plot4](https://github.com/kiinapoika/RepData_PeerAssessment1/blob/master/plot4.png)<!-- -->
