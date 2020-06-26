---
title: "Week 2 Reproducible Research Project: Peer Assessment 1"
author: "C Barclay"
date: "26/06/2020"
output: 
        html_document:
                keep_md: true
---

Before we start, lets load the necessary packages!


```r
# Load  packages
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(readr)
library(dplyr)
library(ggplot2)
```

PART 1. START

(a) Loading and preprocessing the data.
(b) Show any code that is needed to.

Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

Process/transform the data (if necessary) into a format suitable for your
analysis.


```r
## Loading and preprocessing the data

# Coursera "activity.zip" file web location
fileUrl <-
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Set and check working directory
wd <- setwd("C:/Downloads/Datasets")

# Download activity.zip" from webpage. (Remember to destfile to local file area!!!)

download.file(fileUrl, destfile = "C:/Downloads/Datasets/activity.zip",
              method = "curl")

# Unzip contents into R environment.
filesUnzip <-
  unzip(
    "activity.zip",
    files = NULL,
    list = TRUE,
    overwrite = TRUE,
    junkpaths = FALSE,
    exdir = wd,
    unzip = "internal",
    setTimes = FALSE
  )
```

UTILITY
As the working directory will be reset when a chunk is finished running we need
to specify the working directory using knitr.


```r
knitr::opts_knit$set(root.dir = 'C:/Downloads/Datasets')
```

PART 2. STEPS PER DAY

For this part of the assignment, you can ignore the missing values in the
dataset.

(a) Calculate the total number of steps taken per day.

If you do not understand the difference between a histogram and a barplot,
research the difference between them.

(b) Make a histogram of the total number of steps taken each day. 
(c) Calculate and report the mean and median of the total number of steps taken per day.


```r
## What is mean total number of steps taken per day? (Ignoring Na Values)

# Load activity.csv into R environment.
activity <- read.csv(filesUnzip$Name[1])

# Quick summary of file.
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Create the variable "activity_omit" with rows containing Na values removed.
activity_omit <- na.omit(activity)

# Tell user the number of rows with NA values in.
na_diff <- nrow(activity)-nrow(activity_omit)
print(paste("The total number of rows with NA is: ",na_diff))
```

```
## [1] "The total number of rows with NA is:  2304"
```

```r
# Group the number of steps by day and find total sum of steps per day; store in
# the variable "act_Days".
act_Days <- summarise(group_by(activity_omit,date),steps = sum(steps))

## Summary of the MEAN & MEDIAN number of steps per day.
summary(act_Days$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
# Create a Histogram using ggplot2.
ggplot(act_Days, aes(steps)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count",low="blue", high="orange") + labs(title="Histogram for Steps")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PeerAssessment_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

PART 3. DAILY ACTIVITY

(a) Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of
the 5-minute interval (x-axis) and the average number of steps taken, averaged
across all days (y-axis).
(b) Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?


```r
## What is the average daily activity pattern?

# Group the column 'steps' by the column 'interval'. Aggregated 'steps' into
# averages within each 5 minute interval. Store as the variable "act_Interval".
act_Interval <- summarize(group_by(activity_omit, interval), steps=mean(steps))

# Create a Line Plot using ggplot2.
ggplot(act_Interval, aes(interval, steps)) + geom_line(colour ="purple")
```

![](PeerAssessment_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## Which 5-minute Interval, on average across all the days in the dataset,
## contains the maximum number of steps?

# Return the row in variable act_Interval for column steps, where steps is at
# maximum.
print(paste("Interval containing the most steps on average: ",act_Interval$interval[which.max(act_Interval$steps)]))
```

```
## [1] "Interval containing the most steps on average:  835"
```

```r
print(paste("Average steps for that interval: ",round(max(act_Interval$steps),digits=2)))
```

```
## [1] "Average steps for that interval:  206.17"
```

PART 4. DEALING WITH NAs

Note that there are a number of days/intervals where there are missing values
(coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce
bias into some calculations or summaries of the data.

(a) Calculate and report the total number of missing values in the dataset (i.e.
the total number of rows with \color{red}{\verb|NA|}NAs). 
(b) Devise a strategy for filling in all of the missing values 
in the dataset. The strategy does not need to be sophisticated. For example, 
you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
(c) Create a new dataset that is equal to the original dataset
but with the missing data filled in. 
(d) Make a histogram of the total number of steps taken each day and 
Calculate and report the mean and median total number of steps taken per day. 
(e) Do these values differ from the estimates from the first 
part of the assignment? What is the impact of imputing missing data 
on the estimates of the total daily number of steps?


```r
## Imputing missing values

# rename the column 'steps' in "act_Interval" to 'mean.steps'.
names(act_Interval)[2] <- "mean.steps"

# Merge variables "activity" & "act_Interval" and store as new variable "act_Impute".
act_Impute <- merge(activity, act_Interval)

# Replace all instances of Na values in the column 'steps' with adjacent values
# in the column 'mean.steps'.
act_Impute$steps[is.na(act_Impute$steps)] <- act_Impute$mean.steps[is.na(act_Impute$steps)]

## Make a Histogram with imputed data.

# As before, group the number of steps by day and find total sum of steps per day; store in
# the variable "act_Days_Imp".
act_Days_Imp <- summarize(group_by(act_Impute, date), steps=sum(steps))

# Summary of the MEAN & MEDIAN number of steps per day.
summary(act_Days_Imp$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
# Create Histogram using ggplot2.
ggplot(act_Days_Imp, aes(steps)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count",low="blue", high="orange") + labs(title="Histogram for Steps with Imputed Data")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PeerAssessment_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## What is the difference between the Imputed Data and Original Data.
print(paste("The mean appears to be unaffected by this simple data imputation. The median is smaller than the original data."))
```

```
## [1] "The mean appears to be unaffected by this simple data imputation. The median is smaller than the original data."
```

PART 5. WEEKDAYS AND WEEKENDS

(a) Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of
some help here. Use the dataset with the filled-in missing values for this part.

(b) Create a new factor variable in the dataset with two levels – “weekday” and
“weekend” indicating whether a given date is a weekday or weekend day. 
(c) Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type =
"l"|}type = "l") of the 5-minute interval (x-axis) and the average number of
steps taken, averaged across all weekday days or weekend days (y-axis). 

See the README file in the GitHub repository to see an example of what this plot
should look like using simulated data.


```r
## Are there differences in activity patterns between weekdays and weekends?

# Using the variable "act_Impute" with imputed data, create a fifth column named
# 'dayofweek' and store converted DateTime values from column 'date' in day
# format.
act_Impute$dayofweek <- weekdays(as.Date(act_Impute$date))

# Create a sixth column called 'weekend' and populate with logical TRUE/FALSE
# values if Saturday OR Sunday are present in the 'dayofweek' column.
act_Impute$weekend <-as.factor(act_Impute$dayofweek=="Saturday"|act_Impute$dayofweek=="Sunday")

# Rename TRUE/FALSE values as Weekday or Weekend based on assigned value.
levels(act_Impute$weekend) <- c("Weekday", "Weekend")



## Build variables for Histogram

# Create the separate variable "act_Weekday" containing only weekdays according to column 'weekend' in variable "act_Impute".
act_Weekday <- act_Impute[act_Impute$weekend=="Weekday",]

# Repeat process of grouping the number of steps by day and find total sum of steps per day; store in
# the variable "act_Weekday_Interval".
act_Weekday_Interval <- summarize(group_by(act_Weekday, interval), steps=mean(steps))

# Create column 'dow' in preparation from plotting.
act_Weekday_Interval$dow <- "Weekday"

# Additionally, create the separate variable "act_Weekend" containing only weekends according to column 'weekend' in variable "act_Impute".
act_Weekend <- act_Impute[act_Impute$weekend=="Weekend",]

# Repeat process of grouping the number of steps by day and find total sum of steps per day; store in
# the variable "act_Weekend_Interval".
act_Weekend_Interval <- summarize(group_by(act_Weekend, interval), steps=mean(steps))

# Create column 'dow' in preparation from plotting.
act_Weekend_Interval$dow <- "Weekend"

# Append the variables "act_Weekday_Interval" and "act_Weekend_Interval" together and store as new variable "act_Int".
act_Int <- rbind(act_Weekday_Interval, act_Weekend_Interval)
act_Int$dow <- as.factor(act_Int$dow)

# Create Line Plots of the number of steps during the Week vs the Weekend using ggplot2.
ggplot(act_Int, aes(interval, steps)) + geom_line() + facet_grid(dow ~ .)
```

![](PeerAssessment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

END OF SCRIPT
