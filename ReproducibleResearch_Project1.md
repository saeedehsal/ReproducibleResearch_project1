This is R markdown for project 1 in course Reproducbile research.

#Auhor : Saeedeh Salimianrizi

##preparing data

```r
data <- read.csv("activity.csv")
head(data)
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
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


##total number of steps taken per day

```r
steps_by_day <- aggregate(steps ~ date, data, FUN = sum)
```

##histogram of the total number of steps taken each day

```r
hist(steps_by_day$steps, breaks = 10, col = "blue", main = "Total Steps Per Day", xlab = "Number of Steps")
```

![plot of chunk hist](figure/hist-1.png)

##mean and median of the total number of steps taken per day

```r
mean(steps_by_day$steps , na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_by_day$steps , na.rm= TRUE)
```

```
## [1] 10765
```

## time series plot of the 5-minute interval and and average number of steps taken

```r
average_steps_by_interval <- aggregate(steps ~ interval, data, FUN = mean)
plot(average_steps_by_interval$interval, average_steps_by_interval$steps, type = "l", main = "Average Steps~Intervals",xlab = "Intervals", ylab = "Average Steps")
```

![plot of chunk time series plot](figure/time series plot-1.png)


##5-minute interval with the max number of steps

```r
max_average_step <- max(average_steps_by_interval$steps)
average_steps_by_interval[average_steps_by_interval$steps == max_average_step,]
```

```
##     interval    steps
## 104      835 206.1698
```


##total number of missing values in the data set

```r
sum(is.na(data))
```

```
## [1] 2304
```


##impute data, replacing NA values with the mean values of the same interval

```r
data_noNA <- transform(data, steps = ifelse(is.na(data$steps), average_steps_by_interval$steps[match(data$interval, average_steps_by_interval$interval)], data$steps))
steps_by_day_noNA <- aggregate(steps ~ date, data_noNA, FUN = sum)
```


##hist of real data

```r
hist(steps_by_day$steps,  breaks = 10, col = "blue", main = "Total Steps Per Day", xlab = "Number of Steps")
```

![plot of chunk comparing hist of real and imputed data](figure/comparing hist of real and imputed data-1.png)


## hist of imputed data

```r
hist(steps_by_day_noNA$steps, breaks = 10,  col = "red", main = "Total Steps Per Day", xlab = "Number of Steps")
```

![plot of chunk host of imputed data](figure/host of imputed data-1.png)

### as you can see the histogram of real and imputed data are slightly different

##mean and the median for the total number of steps taken per day using imputed data

```r
mean(steps_by_day_noNA$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_day_noNA$steps)
```

```
## [1] 10766.19
```

### as you can see the mean value stayed the same after imputing data but median increased slightly.
##identify weekday and weekend related data

```r
data_noNA$weekday <- ifelse(weekdays(as.Date(data_noNA$date))%in%c("Sunday", "Saturday"), "Weekend", "Workday")
```


## time series plot of the 5-minute interval  and the average number of steps takeN for weekday days or weekend days

```r
average_steps_by_interval_noNA <- aggregate(steps ~ interval + weekday, data_noNA, FUN = mean)

library(lattice)
xyplot(average_steps_by_interval_noNA$steps ~ average_steps_by_interval_noNA$interval|average_steps_by_interval_noNA$weekday, type = "l", 
layout = c(1,2), xlab = "Interval", ylab= "Average Steps Per Day", main = "Average Steps per Day by Interval")
```

![plot of chunk time series](figure/time series-1.png)
