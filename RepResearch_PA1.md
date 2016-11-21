# Reproducible Research: Peer Assessment 1
# About

This was the first project for the Reproducible Research course in Coursera's Data Science specialization track. The purpose of the project was to answer a series of questions using 
[Activity monitoring data] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

# The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken


# Loading the initial data using read.csv() 

```r
unzip(zipfile="activity.zip")    
```

```
## Warning in unzip(zipfile = "activity.zip"): error 1 in extracting from zip
## file
```

```r
data <- read.csv("activity.csv")
```

# Finding Mean and Median of total steps taken each day

```r
library(ggplot2)  
totalSteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)  
qplot(totalSteps, binwidth=500, xlab="Total number of steps taken each day")  
```

![](RepResearch_PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(totalSteps, na.rm=TRUE)  
```

```
## [1] 9354.23
```

```r
median(totalSteps, na.rm=TRUE)  
```

```
## [1] 10395
```
# Finding the average number of steps taken each day and 5-minutes interval

```r
library(ggplot2)  
avgSteps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)  
ggplot(data=avgSteps, aes(x=interval, y=steps)) +
    geom_line() +  
    xlab("5-minute interval") +  
    ylab("Average number of steps taken each day")  
```

![](RepResearch_PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
# Imputing missing values   

```r
missingVal <- is.na(data$steps)  
table(missingVal)  
```

```
## missingVal
## FALSE  TRUE 
## 15264  2304
```
# Replace each missing value with the mean value of its 5-minute interval  

```r
fill.value <- function(steps, interval) {  
    filled <- NA  
    if (!is.na(steps))  
        filled <- c(steps)  
    else  
        filled <- (avgSteps[avgSteps$interval==interval, "steps"])  
    return(filled)  
}  
filled.data <- data 
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)  

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)  
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")  
```

![](RepResearch_PA1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(total.steps)  
```

```
## [1] 10766.19
```

```r
median(total.steps)  
```

```
## [1] 10766.19
```
# Are there differences in activity patterns between weekdays and weekends?  

```r
weekday.or.weekend <- function(date) {  
    day <- weekdays(date)  
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))  
        return("weekday")  
    else if (day %in% c("Saturday", "Sunday"))  
        return("weekend")  
    else  
        stop("invalid date")    
}  
filled.data$date <- as.Date(filled.data$date)  
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)  
```
# Plots for avg number of steps taken on weekdays and weekend. 

```r
avgSteps <- aggregate(steps ~ interval + day, data=filled.data, mean)  
ggplot(avgSteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +  
    xlab("5-minute interval") + ylab("Number of steps")  
```

![](RepResearch_PA1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
