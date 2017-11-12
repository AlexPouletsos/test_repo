---
output: 
  html_document: 
    keep_md: yes
---
Reproducible Research: Peer Assessment 1
==================================


###Loading and preprocessing the data

Code for reading in the dataset and removing NA values


```r
#activity.csv must be in working directory
activityRead <- read.csv("activity.csv")
activity <- activityRead[is.na(activityRead[,1])==FALSE,] #creates dataset with NAs removed
```


###What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day


```r
library(dplyr)
#total steps by date
stepsSumDate <- activity %>%
		group_by(date) %>%
		summarize(total=sum(steps))
hist(stepsSumDate$total, xlab="Total Steps", main="Total Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and median number of steps taken each day

```r
summarize(activity, mean(steps))
```

```
##   mean(steps)
## 1     37.3826
```

```r
summarize(activity, median(steps))
```

```
##   median(steps)
## 1             0
```

 
###What is the average daily activity pattern?
Time series plot of the average number of steps taken

```r
#total steps by interval
stepsSumInt <- activity %>%
		group_by(interval) %>%
		summarize(total=sum(steps))

#average steps by interval
intMeans <- activity %>%
	group_by(interval) %>%
	summarize(mean_steps=mean(steps)) %>%
	arrange(desc(mean_steps))
intMaxMean <- intMeans[1,1] #interval with most steps on average

plot(stepsSumInt$interval, stepsSumInt$total, type="l",
        xlab="Interval", ylab="Total Steps", main="Average Steps Taken Per Interval")
abline(v=intMaxMean, col="red")
axis(side=1, at=intMaxMean)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The 5-minute interval that, on average, contains the maximum number of steps

```r
as.data.frame(intMaxMean)
```

```
##   interval
## 1      835
```

 
###Imputing missing values

The total number of missing values in the dataset

```r
nrow(filter(activityRead, is.na(steps)))
```

```
## [1] 2304
```

Create new dataset by replacing missing values with mean of that interval

```r
imputed <- activityRead %>% 
		group_by(interval) %>% 
		mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

Histogram of the total number of steps taken each day after missing values are imputed

```r
stepsSum <- activity %>%
		group_by(date) %>%
		summarize(total=sum(steps))
stepsSumImp <- imputed %>%
		group_by(date) %>%
		summarize(total=sum(steps))

hist(stepsSumImp$total, xlab="Total Steps", main="Total Steps Per Day", col="red")
hist(stepsSum$total, xlab="", main="", col="grey", add=TRUE)
legend("topleft", bty="n", legend = c("Increase from original data set"),
       pch=22, pt.bg="red", text.col="black", cex=.8)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Calculate the mean, median and total number of steps taken per day

```r
Sum <- c(sum(activity$steps),sum(imputed$steps))
Mean <- c(mean(activity$steps),mean(imputed$steps))
Median <- c(median(activity$steps),median(imputed$steps))
n <- c("Original","Imputed")
data.frame(Sum, Mean, Median ,row.names=n)
```

```
##               Sum    Mean Median
## Original 570608.0 37.3826      0
## Imputed  656737.5 37.3826      0
```
 
###Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend"

```r
#add column for weekday & weekend called wkType
imputed <- as.data.frame(imputed)
imputed2 <- mutate(imputed, weekday = weekdays(as.Date(imputed$date)))
imputed2 <- mutate(imputed2, wkType = ifelse(weekday=="Saturday"|weekday=="Sunday",
		"Weekend","Weekday"))
```
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
library(lattice)
#average steps by wkType and interval
meanWkInt <- imputed2 %>%
		group_by(wkType,interval) %>%
		summarize(AvgSteps=mean(steps))

xyplot(AvgSteps~interval | wkType, data=meanWkInt, type="l",
       xlab="Interval", ylab="Average Steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
