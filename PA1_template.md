# Reproducible Research: Peer Assessment 1
Kevin DeCatur  
Wednesday, May 13, 2015  

```r
library(ggplot2)
library(data.table)
echo = TRUE 
```

### Loading and preprocessing the data

1. Load the data (i.e. `read.csv()`)


```r
data <- read.csv("activity.csv", header = T, colClasses = c("integer", "Date", "factor"))
```

2. Process/transform the data


```r
dataTable = data.table(data)
dataTableByDay = dataTable[, list(steps = sum(steps, na.rm = T)), 
                          by = date]
```

### What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
hist(dataTableByDay$steps, 
     breaks = 50,
     main = "Histogram of the Total Number of Steps",
     xlab = 'Total Number of Steps', col = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(dataTableByDay$steps)
```

```
## [1] 9354.23
```

```r
median(dataTableByDay$steps)
```

```
## [1] 10395
```


### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dataTableMeanByInterval =   dataTable[, list(avgSteps = mean(steps, na.rm = T)), 
                            by = interval]

plot(dataTableMeanByInterval$interval, dataTableMeanByInterval$avgSteps, type = 'l',
             main = 'Average Steps by Interval',
             xlab = 'Interval',
             ylab = 'Average Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps = dataTableMeanByInterval[which.max(avgSteps), interval]
maxSteps
```

```
## [1] 835
## 288 Levels: 0 10 100 1000 1005 1010 1015 1020 1025 1030 1035 1040 ... 955
```

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(dataTable$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
dataTable2 <- dataTable 
for (i in 1:nrow(dataTable2)) {
    if (is.na(dataTable2$steps[i])) {
        dataTable2$steps[i] <- round(dataTableMeanByInterval[which(dataTable2$interval[i] == dataTableMeanByInterval$interval), ]$avgSteps)
     
    }
}

dataTable2ByDay = dataTable2[, list(steps = sum(steps)), 
                          by = date]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(dataTable2ByDay$steps, 
     breaks = 50,
     main = "Histogram of the Total Number of Steps",
     xlab = 'Total Number of Steps', col = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 



```r
mean(dataTable2ByDay$steps)
```

```
## [1] 10765.64
```

```r
median(dataTable2ByDay$steps)
```

```
## [1] 10762
```

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


