  
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
editor_options: 
  chunk_output_type: console
---



## Loading and preprocessing the data

1. Reading the data

```r
data <- read.csv("activity.csv")
```

2. Transform the variable date with date format

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## Total number of steps taken per day

```r
# Filter NAs
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
good <- !(is.na(data$steps))
data2 <- data[good,]
```

1. Total number of steps taken per day

```r
steps_total <- data2 %>% group_by(date) %>% summarise(total = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_total
```

```
## # A tibble: 53 x 2
##    date       total
##    <date>     <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```


2. Histogram of the total number of steps taken per day

```r
hist(steps_total$total, xlab = "Steps per day", main = "Histogram of total number steps taken per day", col = "dark green", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


3. Obtaining the mean and the median number of steps by day

```r
agg <- (data2 %>% group_by(date) 
    %>% summarise(mean_steps = mean(steps), median_steps = median(steps)) 
    %>% print)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 53 x 3
##    date       mean_steps median_steps
##    <date>          <dbl>        <dbl>
##  1 2012-10-02      0.438            0
##  2 2012-10-03     39.4              0
##  3 2012-10-04     42.1              0
##  4 2012-10-05     46.2              0
##  5 2012-10-06     53.5              0
##  6 2012-10-07     38.2              0
##  7 2012-10-09     44.5              0
##  8 2012-10-10     34.4              0
##  9 2012-10-11     35.8              0
## 10 2012-10-12     60.4              0
## # ... with 43 more rows
```


## Daily activity pattern


1. Creating a series plot


First, the data needs to be aggregated by the mean for each interval

```r
library(ggplot2)
data_plot <- (data2 %>% group_by(interval) 
    %>% summarise(mean_steps = mean(steps)) 
    %>% print)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 288 x 2
##    interval mean_steps
##       <int>      <dbl>
##  1        0     1.72  
##  2        5     0.340 
##  3       10     0.132 
##  4       15     0.151 
##  5       20     0.0755
##  6       25     2.09  
##  7       30     0.528 
##  8       35     0.868 
##  9       40     0     
## 10       45     1.47  
## # ... with 278 more rows
```

```r
# Creating the plot
g <- ggplot(data_plot, aes(x = interval, y = mean_steps))
g + geom_line(color = "blue", size = 1) + labs(title = "Average daily activity pattern by intervals", x = "Interval", y = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


2. Getting the interval with max number of steps

```r
n <- (data2 %>% group_by(interval) 
    %>% summarise(mean_steps = mean(steps))
    %>% summarise(max_n = which.max(mean_steps))
    %>% print)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 1
##   max_n
##   <int>
## 1   104
```

```r
max_num <- data2$interval[n$max_n]
```


On average across all the days in the dataset, the 5 minute interval that contains the maximum number of steps is 835


## Imputing missing values


1. Getting total missing values

```r
missing <- sum(is.na(data))
missing
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304


2. Using the steps mean by interval to fill the observations with missing values

```r
length(which(data$interval == 0)) # There are 61 repeated intervals
```

```
## [1] 61
```


3. Creating a new dataset with the missing data filled in

```r
# New dataset
new_data <- data
# Using the previous data to obtain the mean steps by interval
new_data$mean_steps <- rep(data_plot$mean_steps, 61)

# loop to fill missing values
for (i in 1:nrow(new_data)) {
    if (is.na(new_data$steps[i])){
        new_data$steps[i] <- new_data$mean_steps[i]
    } else{next}
}
```


4. Obtaining a histogram with the new dataset

```r
totals <- new_data %>% group_by(date) %>% summarise(total = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(totals, hist(total, xlab = "Steps per day", main = "Total number steps taken per day", col = "blue", breaks = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
agg2 <- (new_data %>% group_by(date) 
    %>% summarise(mean_steps = mean(steps), median_steps = median(steps)) 
    %>% print)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 61 x 3
##    date       mean_steps median_steps
##    <date>          <dbl>        <dbl>
##  1 2012-10-01     37.4           34.1
##  2 2012-10-02      0.438          0  
##  3 2012-10-03     39.4            0  
##  4 2012-10-04     42.1            0  
##  5 2012-10-05     46.2            0  
##  6 2012-10-06     53.5            0  
##  7 2012-10-07     38.2            0  
##  8 2012-10-08     37.4           34.1
##  9 2012-10-09     44.5            0  
## 10 2012-10-10     34.4            0  
## # ... with 51 more rows
```


## Are there differences in activity patterns between weekdays and weekends?


1. Creating a new factor variable with two levels: "weekday" and "weekend"

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
week1 = weekdays(data$date)

# Transforming the variable weekdays into two levels
week1[week1 %in% c("lunes","martes","miércoles","jueves","viernes")] <- "weekday"
week1[week1 %in% c("sábado","domingo")] <- "weekend"

new_data$weekdays <- factor(week1)
str(new_data)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps     : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date      : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean_steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ weekdays  : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


2. Creating a series plot by each level

```r
data_plot2 <- (new_data %>% group_by(weekdays,interval) 
    %>% summarise(mean_steps = mean(steps)) 
    %>% print)
```

```
## `summarise()` regrouping output by 'weekdays' (override with `.groups` argument)
```

```
## # A tibble: 576 x 3
## # Groups:   weekdays [2]
##    weekdays interval mean_steps
##    <fct>       <int>      <dbl>
##  1 weekday         0     2.25  
##  2 weekday         5     0.445 
##  3 weekday        10     0.173 
##  4 weekday        15     0.198 
##  5 weekday        20     0.0990
##  6 weekday        25     1.59  
##  7 weekday        30     0.693 
##  8 weekday        35     1.14  
##  9 weekday        40     0     
## 10 weekday        45     1.80  
## # ... with 566 more rows
```

```r
g2 <- ggplot(data_plot2, aes(x = interval, y = mean_steps))
g2 + geom_line(color = "red") + facet_wrap(. ~ weekdays, nrow = 2) + labs(title = "Daily activity pattern by intervals", x = "Interval", y = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
