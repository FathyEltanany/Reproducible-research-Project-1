# PA1-template
Fathy Eltanany  
June 13, 2017  

Reproducible Research: Course Project 1
=================================================
## Loading Data

```r
data<- read.csv("activity.csv")
```

## Mean total number of steps taken per day 


```r
library(ggplot2)
steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(steps, binwidth=1000, xlab="total number of steps")
```

![](PA1-template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(steps,na.rm = TRUE)
```

```
## [1] 10395
```

## The average daily activity pattern

```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1-template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Maximum number of steps in 5 minutes interval


```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
### total number of missing values in the dataset 


```r
missing <- is.na(data$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```


### fill in with mean value for that 5-minute interval.

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}

#create new data set without NAs
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

### Making the histogram 

```r
steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(steps, binwidth=1000, xlab="total number of steps taken each day")
```

![](PA1-template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
