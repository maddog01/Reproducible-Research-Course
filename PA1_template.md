

---
title: "Activity Monitoring Project"
output: html_document
---



## Importing the data


The following r code is how we input the data from the activity excel doc:


```r
data <- read.csv("activity.csv",  stringsAsFactors = TRUE)
```

## Histograph of total steps per day

 We begin by creating a histograph of the Number of Steps per day:


```r
sub_data <- tapply(data$steps, data$date, sum, na.rm = TRUE)

hist(sub_data, col = "green", main = "graph of steps per day", xlab="sum of steps by day")
```

![plot of chunk unnamed-chunk-14](https://github.com/maddog01/Reproducible-Research-Course/blob/master/unnamed-chunk-14-1.png)
## Finding the mean and median of the data

The following r code is how we calculate the mean and median of the total number of steps taken per day:


```r
mean1 <- mean(sub_data)
median1 <- median(sub_data)

print(c(mean1, median1))
```

```
## [1]  9354.23 10395.00
```
##Time series plot
We now create a time series plot of the 5-minute interal (x-axis) and the average number of steps taken, averages across all days (y-axis):


```r
sub_data1 <- data[complete.cases(data[,"steps"]),]

int_avg <- tapply(sub_data1$steps, sub_data1$interval, mean, na.rm=T)


plot(int_avg ~ unique(sub_data1$interval), type = "l", xlab = "interval", ylab = "avg number of steps")
```

![plot of chunk unnamed-chunk-16](https://github.com/maddog01/Reproducible-Research-Course/blob/master/unnamed-chunk-16-1.png)
We now find the interval with the most steps


```r
t <- which.max(int_avg)

int_avg[t]
```

```
##      835 
## 206.1698
```

##Considering missing values

We need to consider the amount and affect of the missing values from the data set. It is possible that the missing values may be causing bias results in our data.

We will begin by calculating the total number of missing values:


```r
sum(is.na(data))
```

```
## [1] 2304
```
We can assume that the NA values all only exist in the "step" column of the data set because this is the only variable that required human action. The dates and 5-minute intervals would occur regardless of what the individual did or did not do.

So lets fill in the missing NA values with the mean average of each interval for that day. This is the data that we have stored in "int_avg".


```r
comp_data <- data

for (i in 1:nrow(data)){
        if(is.na(data$steps[i]))
        {comp_data$steps[i] <- int_avg[as.character(data$interval[i])]}
}
```
We will now create a histograph of the data with the NA values filled in:



```r
sub_comp_data <- tapply(comp_data$steps, comp_data$date, sum, na.rm = TRUE)

hist(sub_comp_data, col = "blue", main = "graph of steps per day", xlab="sum of steps by day")
```

![plot of chunk unnamed-chunk-20](https://github.com/maddog01/Reproducible-Research-Course/blob/master/unnamed-chunk-20-1.png)

We will now calculate the new mean and median of this new data set:



```r
mean2<- mean(sub_comp_data)
median2 <- median(sub_comp_data)
print(c(mean2, median2))
```

```
## [1] 10766.19 10766.19
```

How do these vaules compare to the first data set?



```r
abs(mean1-mean2)
```

```
## [1] 1411.959
```

```r
abs(median1-median2)
```

```
## [1] 371.1887
```

So we see that the averages from the 2nd data set do differ slightly from the 1st data set. Lets compare the two graphs:


```r
par(mfrow =c(1,2))

hist(sub_data, col = "green", main = "steps data 1", xlab="sum of steps by day")

hist(sub_comp_data, col = "blue", main = "steps data 2", xlab="sum of steps by day")
```

![plot of chunk unnamed-chunk-23](https://github.com/maddog01/Reproducible-Research-Course/blob/master/unnamed-chunk-23-1.png)

So there does appear to be some difference between the graph confirming that the missing values do impact certain parts of the data but overall, the difference is small.

##Differences between weekdays and weekends

We will now explore the filled in data for any potential differences between the weekdays and the weekends. 

First, we will create a new variable which will seperate the data into "weekday and weekend" days.


```r
comp_data$date <- as.POSIXct(comp_data$date)

comp_data$daytype <- ifelse(weekdays(comp_data$date)%in% c("Saturday", "Sunday"), "weekend", "weekday")
  
sub_wd <- subset.data.frame(comp_data, comp_data$daytype %in% "weekday")

sub_we <- subset.data.frame(comp_data, comp_data$daytype %in% "weekend")


int_avg_wd <- tapply(sub_wd$steps, sub_wd$interval, mean, na.rm=T)
int_avg_we <- tapply(sub_we$steps, sub_we$interval, mean, na.rm=T)

par(mfrow =c(2,1))

plot(int_avg_wd ~ unique(sub_wd$interval), type = "l", xlab = "interval", ylab = "avg weekday steps")

plot(int_avg_we ~ unique(sub_we$interval), type = "l", xlab = "interval", ylab = "avg weekend steps")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

From the graphs, we see that the individual was more active during the weekend days than they were during the weekday days.

