# Reproducible Research: Peer Assessment 1  

### Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.


```r
data$intervalNumbs <- as.factor(data$interval)
levels(data$intervalNumbs) <- 1:(length(unique(data$interval)))
```

### What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
date <- vector()
total <- vector()
for (i in levels(data$date)) {
        date <- c(date, i)
        total <- c(total, sum(data[data$date==i,]$steps, na.rm=TRUE))
}
hist(total, xlab="Total Steps", main="Histogram of Total Daily Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(total)
```

```
## [1] 9354
```

```r
median(total)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageSteps <- vector()
for (i in levels(data$intervalNumbs)) {
        averageSteps <- c(averageSteps, mean(data[data$intervalNumbs==i,]$steps, na.rm=TRUE))
}
plot(levels(data$intervalNumbs), averageSteps, type="l", xlab="Hour of Day", ylab="Average Steps", xaxt="n", main="Month Averaged Steps for Each 5 Minute Interval in the Day")
axis(side=1, at=seq(1,length(levels(data$intervalNumbs)),12), labels=0:23)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
cat("Hour ", (which.max(averageSteps) - which.max(averageSteps) %% 12) /12)
```

```
## Hour  8
```

```r
cat("Minute ", (which.max(averageSteps) %% 12)*5)
```

```
## Minute  40
```

### Imputing missing values

1. Calculating the total number of missing values in the data set.


```r
nrow(data[data$steps=="NA",])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Going to use the mean for that 5-minute interval*

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Save copy of old data before imputing NA values.
oldData <- data
for (i in 1:nrow(data)) {
        if (is.na(data[i,]$steps)) {
                data[i,]$steps <- averageSteps[data[i,]$intervalNumbs]
        }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newDate <- vector()
newTotal <- vector()
for (i in levels(data$date)) {
        newDate <- c(newDate, i)
        newTotal <- c(newTotal, sum(data[data$date==i,]$steps))
}
hist(newTotal, xlab="Total Steps", main="Histogram of Total Daily Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean(newTotal)
```

```
## [1] 10766
```

```r
median(newTotal)
```

```
## [1] 10766
```

*Imputing missing values has the effect of raising estimates of total number of daily steps.*

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data$weekday <- weekdays(as.Date(data$date))
data$weekend <- data$weekday == "Saturday" | data$weekday == "Sunday"
data$weekend <- as.factor(data$weekend)
levels(data$weekend) <- c("weekday", "weekend")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
par(mfcol=c(1,2))
averageSteps <- vector()
for (i in levels(data$intervalNumbs)) {
        averageSteps <- c(averageSteps, mean(data[data$intervalNumbs==i & data$weekend=="weekday",]$steps, na.rm=TRUE))
}
plot(levels(data$intervalNumbs), averageSteps, type="l", xlab="Hour of Day", ylab="Average Steps", xaxt="n", main="Month Averaged Steps for Each \n5 Minute Interval of a Weekday")
axis(side=1, at=seq(1,length(levels(data$intervalNumbs)),12), labels=0:23)

averageSteps <- vector()
for (i in levels(data$intervalNumbs)) {
        averageSteps <- c(averageSteps, mean(data[data$intervalNumbs==i & data$weekend=="weekend",]$steps, na.rm=TRUE))
}
plot(levels(data$intervalNumbs), averageSteps, type="l", xlab="Hour of Day", ylab="Average Steps", xaxt="n", main="Month Averaged Steps for Each \n5 Minute Interval of a Weekend \nDay")
axis(side=1, at=seq(1,length(levels(data$intervalNumbs)),12), labels=0:23)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
