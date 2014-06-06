# Reproducible Research: Peer Assessment 1

In this assignment/Rmarkdown file we load some data, perform some preprocessing steps, calculate some measures and produce several plots.  

## Loading and preprocessing the data

Load the data using the read.csv command and take a look at the first few rows.  

```r
library(data.table)
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

Next we perform some simple preprocessing:
* Convert the steps column to numeric
* Convert the date column to R's date objects
* Convert the interval column to factors
* Add a column representing the days as a factor value starting at 1.


```r
data$steps    <- as.numeric(data$steps)
data$date     <- strptime(data$date,"%Y-%m-%d")
data$date     <- as.Date(data$date)
numints       <- length(unique(data$interval))   # Total number of intervals
days          <- (as.numeric(format(data$date,"%m"))-10)*31 + 
  as.numeric(format(data$date,"%d"))
numdays       <- max(days)            # Total number of days
data          <- cbind(data,days) 
```


## What is the mean total number of steps taken per day?

In order to calculate the mean total number of steps per day we first convert the 
data to a data.table object.  There are a total of 61 days.  We first display the full length vectors of the mean and median per day.  If there were no step values in a day then the computation returns an NA, otherwise NAs are ignored.  Additionally, we plot a histogram of the total number of steps taken each day. 


```r
data       <- data.table(data)
steps.days <- data[,list(
  sum    = sum(steps,na.rm=TRUE),
  mean   = mean(steps,na.rm=TRUE),
  median = median(steps,na.rm=TRUE)),
  by=days]
steps.days$mean
```

```
##  [1]     NaN  0.4375 39.4167 42.0694 46.1597 53.5417 38.2465     NaN
##  [9] 44.4826 34.3750 35.7778 60.3542 43.1458 52.4236 35.2049 52.3750
## [17] 46.7083 34.9167 41.0729 36.0938 30.6285 46.7361 30.9653 29.0104
## [25]  8.6528 23.5347 35.1354 39.7847 17.4236 34.0938 53.5208     NaN
## [33] 36.8056 36.7049     NaN 36.2465 28.9375 44.7326 11.1771     NaN
## [41]     NaN 43.7778 37.3785 25.4722     NaN  0.1424 18.8924 49.7882
## [49] 52.4653 30.6979 15.5278 44.3993 70.9271 73.5903 50.2708 41.0903
## [57] 38.7569 47.3819 35.3576 24.4688     NaN
```

```r
steps.days$median
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```

```r
hist(steps.days$sum, 
     xlab  = "Sum of Steps per Day",
     main  = "Histogram Showing the Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## What is the average daily activity pattern?

The interval column gives a way to calculate the daily activity.  Below is the R-code that calculates the means per interval, which gives us an activity pattern.  There are 288 intervals in each day.  We plot the activity pattern in a time-series plot.  


```r
actpat <- data[,list(
  mean=mean(steps,na.rm=TRUE),
  median=median(steps,na.rm=TRUE)),
  by=interval]
plot(actpat$interval,actpat$mean,
     type = "l",
     xlab = "Intervals",
     ylab = "Mean Number of Steps",
     main = "Mean Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
maxmean     <- max(actpat$mean )
maxind      <- which( actpat$mean == maxmean)
maxinterval <- actpat$interval[maxind]
```

The 835th interval has the maximum value of the means across all the days in the dataset.  The maximum value is 206.1698.


## Inputing missing values

There are NA values in the data.  In this part of the report we input appropriate values for the missing values.   

```r
numNA  <- sum(as.numeric(is.na(data$steps)))
percNA <- round(numNA/dim(data)[1]*100,digits = 1)
#sum(as.numeric(is.na(data$date)))
#sum(as.numeric(is.na(data$interval)))
```
There are a total of 2304 NAs in the dataset.  These occur only in the steps column, and this constitutes 13.1 percent of the rows. 

We fill-in any NAs by filling in the mean over all the days for given 5-minute interval. The filled in dataset is called data2, instead of data. 

```r
data2        <- data                  # Copy of the data
rinds         <- which(is.na(data$steps) == TRUE)
# rinds        <- is.na(data$steps)     # Indices need to be replaced
rintervals   <- data$interval[rinds]  # Intervals that have NAs
for (n in 1:numints)
{
  tmp_inds   <- (actpat$interval[n] == rintervals)
  data2$steps[rinds[tmp_inds]] <- actpat$mean[n]  
}
```

We show a histogram of the total number of steps taken each day below.  Additionally, we calculate and show the mean and median total number of steps taken per day.

```r
steps.days2 <- data2[,list(
  sum    = sum(steps,na.rm=TRUE),
  mean   = mean(steps,na.rm=TRUE),
  median = median(steps,na.rm=TRUE)),
  by=days]
steps.days2$mean
```

```
##  [1] 37.3826  0.4375 39.4167 42.0694 46.1597 53.5417 38.2465 37.3826
##  [9] 44.4826 34.3750 35.7778 60.3542 43.1458 52.4236 35.2049 52.3750
## [17] 46.7083 34.9167 41.0729 36.0938 30.6285 46.7361 30.9653 29.0104
## [25]  8.6528 23.5347 35.1354 39.7847 17.4236 34.0938 53.5208 37.3826
## [33] 36.8056 36.7049 37.3826 36.2465 28.9375 44.7326 11.1771 37.3826
## [41] 37.3826 43.7778 37.3785 25.4722 37.3826  0.1424 18.8924 49.7882
## [49] 52.4653 30.6979 15.5278 44.3993 70.9271 73.5903 50.2708 41.0903
## [57] 38.7569 47.3819 35.3576 24.4688 37.3826
```

```r
steps.days2$median
```

```
##  [1] 34.11  0.00  0.00  0.00  0.00  0.00  0.00 34.11  0.00  0.00  0.00
## [12]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
## [23]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00 34.11  0.00
## [34]  0.00 34.11  0.00  0.00  0.00  0.00 34.11 34.11  0.00  0.00  0.00
## [45] 34.11  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
## [56]  0.00  0.00  0.00  0.00  0.00 34.11
```

```r
hist(steps.days2$sum, 
     xlab = "Sum of Steps per Day",
     main = "Histogram (with NAs removed")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Follow-up questions:
* Do these values differ from the estimates from the first part of the assignment?
* What is the impact of imputing missing data on the estimates of the total daily number of steps?

Below we calculate the differences between the means, medians and sums.  

```r
abs(steps.days$mean - steps.days2$mean) 
```

```
##  [1] NaN   0   0   0   0   0   0 NaN   0   0   0   0   0   0   0   0   0
## [18]   0   0   0   0   0   0   0   0   0   0   0   0   0   0 NaN   0   0
## [35] NaN   0   0   0   0 NaN NaN   0   0   0 NaN   0   0   0   0   0   0
## [52]   0   0   0   0   0   0   0   0   0 NaN
```

```r
abs(steps.days$median - steps.days2$median)
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```

```r
abs(steps.days$sum - steps.days2$sum)
```

```
##  [1] 10766     0     0     0     0     0     0 10766     0     0     0
## [12]     0     0     0     0     0     0     0     0     0     0     0
## [23]     0     0     0     0     0     0     0     0     0 10766     0
## [34]     0 10766     0     0     0     0 10766 10766     0     0     0
## [45] 10766     0     0     0     0     0     0     0     0     0     0
## [56]     0     0     0     0     0 10766
```
The estimates are different, but the values are only different on days corresponding to ones that had NA values.  The total daily number of steps is increased, because there are now values at the indices that had NA before.  These different values are all the same, i.e. 1.0766 &times; 10<sup>4</sup>, which is the sum of the average values on each interval.   

## Are there differences in activity patterns between weekdays and weekends?

We first construct a factor variable differentiating between weekdays and weekend days.  The code for this is below.

```r
chcks   <- (weekdays(data2$date) == "Saturday") |
           (weekdays(data2$date) == "Sunday")
chcks[chcks==TRUE]  <- "weekend"
chcks[chcks==FALSE] <- "weekday"
daytype <- as.factor(chcks)
data2   <- cbind(data2,daytype)
```

Next, we averavage over all 5-minute invertals (across all weekdays and separately across weekend days).  These two plots are shown below.  


```r
actpat_weekend <- data2[
  daytype == "weekend",list(
  mean=mean(steps,na.rm=TRUE),
  median=median(steps,na.rm=TRUE)),
  by=interval]
actpat_weekday <- data2[
  daytype == "weekday",list(
  mean=mean(steps,na.rm=TRUE),
  median=median(steps,na.rm=TRUE)),
  by=interval]
par(mfrow = c(2,1))
plot(actpat_weekend$interval,
     actpat_weekend$mean,
     type = "l",
     xlab = "Intervals",
     ylab = "Mean Number of Steps",
     main = "Weekend")
plot(actpat_weekday$interval,
     actpat_weekday$mean,
     type = "l",
     xlab = "Intervals",
     ylab = "Mean Number of Steps",
     main = "Weekdaays")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
maxmean     <- max(actpat_weekday$mean )
maxind      <- which( actpat_weekday$mean == maxmean)
maxinterval <- actpat_weekday$interval[maxind]
```
We can clearly see that there is a difference in when steps are being made between weekends and weekdays.  We could hypothesize that on the weekends the steps are much more uniform because the person is relatively active throughout the days.  In contrast, on the weekdays there is a peak in the morning, 835, and otherwise it is pretty low.  So, this person may exercise before work and otherwise stay pretty sedentary. 
