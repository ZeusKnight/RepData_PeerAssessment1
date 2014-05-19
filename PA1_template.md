# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data = read.csv("activity.csv")
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
data_no_na = data[which(!is.na(data$steps)), ]
head(data_no_na)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?


```r
ggplot(data_no_na, aes(as.Date(data_no_na$date), data_no_na$steps)) + geom_bar(stat = "identity") + 
    scale_x_date(breaks = "7 days", labels = date_format("%Y-%m-%d")) + xlab("Date") + 
    ylab("Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean_date = data.frame(mean = sapply(unique(data_no_na$date), function(x) mean(data_no_na[which(data_no_na$date == 
    x), ]$steps)), date = unique(data_no_na$date))
summary(mean_date)
```

```
##       mean               date   
##  Min.   : 0.14   2012-10-02: 1  
##  1st Qu.:30.70   2012-10-03: 1  
##  Median :37.38   2012-10-04: 1  
##  Mean   :37.38   2012-10-05: 1  
##  3rd Qu.:46.16   2012-10-06: 1  
##  Max.   :73.59   2012-10-07: 1  
##                  (Other)   :47
```

```r
mean_date
```

```
##       mean       date
## 1   0.4375 2012-10-02
## 2  39.4167 2012-10-03
## 3  42.0694 2012-10-04
## 4  46.1597 2012-10-05
## 5  53.5417 2012-10-06
## 6  38.2465 2012-10-07
## 7  44.4826 2012-10-09
## 8  34.3750 2012-10-10
## 9  35.7778 2012-10-11
## 10 60.3542 2012-10-12
## 11 43.1458 2012-10-13
## 12 52.4236 2012-10-14
## 13 35.2049 2012-10-15
## 14 52.3750 2012-10-16
## 15 46.7083 2012-10-17
## 16 34.9167 2012-10-18
## 17 41.0729 2012-10-19
## 18 36.0938 2012-10-20
## 19 30.6285 2012-10-21
## 20 46.7361 2012-10-22
## 21 30.9653 2012-10-23
## 22 29.0104 2012-10-24
## 23  8.6528 2012-10-25
## 24 23.5347 2012-10-26
## 25 35.1354 2012-10-27
## 26 39.7847 2012-10-28
## 27 17.4236 2012-10-29
## 28 34.0938 2012-10-30
## 29 53.5208 2012-10-31
## 30 36.8056 2012-11-02
## 31 36.7049 2012-11-03
## 32 36.2465 2012-11-05
## 33 28.9375 2012-11-06
## 34 44.7326 2012-11-07
## 35 11.1771 2012-11-08
## 36 43.7778 2012-11-11
## 37 37.3785 2012-11-12
## 38 25.4722 2012-11-13
## 39  0.1424 2012-11-15
## 40 18.8924 2012-11-16
## 41 49.7882 2012-11-17
## 42 52.4653 2012-11-18
## 43 30.6979 2012-11-19
## 44 15.5278 2012-11-20
## 45 44.3993 2012-11-21
## 46 70.9271 2012-11-22
## 47 73.5903 2012-11-23
## 48 50.2708 2012-11-24
## 49 41.0903 2012-11-25
## 50 38.7569 2012-11-26
## 51 47.3819 2012-11-27
## 52 35.3576 2012-11-28
## 53 24.4688 2012-11-29
```

```r
median_date = data.frame(median = sapply(unique(data_no_na$date), function(x) median(data_no_na[which(data_no_na$date == 
    x), ]$steps)), date = unique(data_no_na$date))
summary(median_date)
```

```
##      median          date   
##  Min.   :0   2012-10-02: 1  
##  1st Qu.:0   2012-10-03: 1  
##  Median :0   2012-10-04: 1  
##  Mean   :0   2012-10-05: 1  
##  3rd Qu.:0   2012-10-06: 1  
##  Max.   :0   2012-10-07: 1  
##              (Other)   :47
```

```r
median_date
```

```
##    median       date
## 1       0 2012-10-02
## 2       0 2012-10-03
## 3       0 2012-10-04
## 4       0 2012-10-05
## 5       0 2012-10-06
## 6       0 2012-10-07
## 7       0 2012-10-09
## 8       0 2012-10-10
## 9       0 2012-10-11
## 10      0 2012-10-12
## 11      0 2012-10-13
## 12      0 2012-10-14
## 13      0 2012-10-15
## 14      0 2012-10-16
## 15      0 2012-10-17
## 16      0 2012-10-18
## 17      0 2012-10-19
## 18      0 2012-10-20
## 19      0 2012-10-21
## 20      0 2012-10-22
## 21      0 2012-10-23
## 22      0 2012-10-24
## 23      0 2012-10-25
## 24      0 2012-10-26
## 25      0 2012-10-27
## 26      0 2012-10-28
## 27      0 2012-10-29
## 28      0 2012-10-30
## 29      0 2012-10-31
## 30      0 2012-11-02
## 31      0 2012-11-03
## 32      0 2012-11-05
## 33      0 2012-11-06
## 34      0 2012-11-07
## 35      0 2012-11-08
## 36      0 2012-11-11
## 37      0 2012-11-12
## 38      0 2012-11-13
## 39      0 2012-11-15
## 40      0 2012-11-16
## 41      0 2012-11-17
## 42      0 2012-11-18
## 43      0 2012-11-19
## 44      0 2012-11-20
## 45      0 2012-11-21
## 46      0 2012-11-22
## 47      0 2012-11-23
## 48      0 2012-11-24
## 49      0 2012-11-25
## 50      0 2012-11-26
## 51      0 2012-11-27
## 52      0 2012-11-28
## 53      0 2012-11-29
```

## What is the average daily activity pattern?

```r
ggplot(data_no_na, aes(unique(data_no_na$interval), sapply(unique(data_no_na$interval), 
    function(x) mean(data_no_na[which(data_no_na$interval == x), ]$steps)))) + 
    geom_line(stat = "identity") + xlab("Date") + ylab("Steps")
```

![plot of chunk simulation](figure/simulation.png) 

```r
interval_data = data.frame(mean = sapply(unique(data_no_na$interval), function(x) mean(data_no_na[which(data_no_na$interval == 
    x), ]$steps)), interval = unique(data_no_na$interval))
```

###Maximum number of steps

```r
tail(interval_data[order(interval_data$mean), ], n = 1)$interval
```

```
## [1] 835
```

## Imputing missing values

```r
nrow(data[which(is.na(data$steps)), ])
```

```
## [1] 2304
```

```r

for (i in 1:nrow(data)) {
    if (is.na(data[i, 1])) {
        if (data[[i, 2]] %in% mean_date$date) {
            data[i, 1] = mean_date[which(mean_date$date == x[i, 2]), ]$steps
        } else {
            data[i, 1] = 0
        }
    }
}
summary(data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 32.5   2012-10-04:  288   Mean   :1178  
##  3rd Qu.:  0.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##                  (Other)   :15840
```

## Are there differences in activity patterns between weekdays and weekends?
###I choose to input the mean instead of NA based on each day, if that particular day contains all NA, then use 0

```r
ggplot(data, aes(as.Date(data$date), data$steps)) + geom_bar(stat = "identity") + 
    scale_x_date(breaks = "7 days", labels = date_format("%Y-%m-%d")) + xlab("Date") + 
    ylab("Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
mean_date_new = data.frame(mean = sapply(unique(data$date), function(x) mean(data[which(data$date == 
    x), ]$steps)), date = unique(data$date))
median_date_new = data.frame(median = sapply(unique(data$date), function(x) median(data[which(data$date == 
    x), ]$steps)), date = unique(data$date))
```

###Is it the same for mean?

```r
test1 <- function() {
    c = TRUE
    for (i in 1:nrow(mean_date)) {
        if (mean_date[i, 1] != (mean_date_new[which(mean_date_new$date == mean_date[i, 
            2]), ]$mean)) {
            c = FALSE
            break
        }
    }
    c
}
test1()
```

```
## [1] TRUE
```

###Is it the same for median?

```r
test2 <- function() {
    c = TRUE
    for (i in 1:nrow(median_date)) {
        if (median_date[i, 1] != (median_date_new[which(median_date_new$date == 
            median_date[i, 2]), ]$median)) {
            c = FALSE
            break
        }
    }
    c
}
test2()
```

```
## [1] TRUE
```


###Graph for weekday and weekend:

```r
days = weekdays(as.Date(data$date))

for (i in 1:length(days)) {
    if (days[i] == "Sunday" || days[i] == "Saturday") {
        days[i] <- "weekend"
    } else {
        days[i] <- "weekday"
    }
}

data = cbind(data, days = days)

weekday_data = data[which(data$days == "weekday"), ]
weekend_data = data[which(data$days == "weekend"), ]
weekday_frame = data.frame(steps = sapply(unique(weekday_data$interval), function(x) mean(weekday_data[which(weekday_data$interval == 
    x), ]$steps)), interval = unique(weekday_data$interval), days = c(rep("weekday", 
    288)))
weekend_frame = data.frame(steps = sapply(unique(weekend_data$interval), function(x) mean(weekend_data[which(weekend_data$interval == 
    x), ]$steps)), interval = unique(weekend_data$interval), days = c(rep("weekend", 
    288)))
week_frame = rbind(weekday_frame, weekend_frame)
xyplot(week_frame$steps ~ week_frame$interval | week_frame$days, type = "l", 
    layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

