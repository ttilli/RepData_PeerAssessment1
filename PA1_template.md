# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
df <- read.csv("activity.csv")
df$date <- as.Date(df$date)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

```r
df.completecases <- na.omit(df)
```
1.  Make a histogram of the total number of steps taken each day

```r
steps.by.day <- aggregate(steps ~ date, data=df.completecases, FUN=sum)
ggplot(steps.by.day, aes(x=date, y=steps)) + geom_bar(stat="identity")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


2.  Calculate and report the mean and median total number of steps taken per day

```r
mean(steps.by.day$steps)
```

```
## [1] 10766.19
```

```r
median(steps.by.day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.by.interval <- aggregate(steps ~ interval, data=df.completecases,
                               FUN=mean)
ggplot(steps.by.interval, aes(x=interval, y=steps)) +
  geom_line(stat="identity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.by.interval$interval[which.max(steps.by.interval$steps)]
```

```
## [1] 835
```


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(df))) # Checking for incomplete cases on the entire set
```

```
## [1] 2304
```

```r
length(which(is.na(df$steps))) # Seeing how many NAs are isolated to "steps"
```

```
## [1] 2304
```
2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
mean(na.omit(df$steps))
```

```
## [1] 37.3826
```

```r
median(na.omit(df$steps))
```

```
## [1] 0
```
Using the median to impute values.

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df.new <- df
df.new.median <- median(na.omit(df.new$steps))
df.new$steps[is.na(df.new$steps)] <- df.new.median
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ggplot(df.new, aes(x=date, y=steps)) + geom_bar(stat="identity")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(df.new$steps)
```

```
## [1] 32.47996
```

```r
median(df.new$steps)
```

```
## [1] 0
```
Result: Imputing values slightly lowered the mean and did not change the median.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
df.new$day[weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekend"
df.new$day[!weekdays(as.Date(df.new$date)) %in% c("Saturday", "Sunday")] <- "weekday"
df.new[, 3] <- as.factor(df.new[, 3])
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.

```r
steps.new <- aggregate(steps ~ interval + day, data=df.new, FUN=mean)
steps.new$interval <- as.integer(steps.new$interval)
ggplot(steps.new, aes(x=interval, y=steps, group=1)) + geom_line() +
    facet_wrap(~ day, ncol=1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

