# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(echo=TRUE, warning =FALSE)
```
## Loading and preprocessing the data

```r
actData <- read.csv(file = "activity.csv", stringsAsFactors=FALSE, sep=",")
```
## What is mean total number of steps taken per day?
The histogram for number of steps taken each day is given below

```r
stepsByDate <- as.vector(by(actData$steps, actData$date, FUN=sum))
hist(stepsByDate,
     col='red', 
     main = "Activity Data", 
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/hist-1.png) 

```r
meanStepsByDate <- round(mean(stepsByDate, na.rm=TRUE),2)
medianStepsByDate <- median(stepsByDate,na.rm=TRUE)
```

The mean of number of steps is 10766.19 and median is 10765


## What is the average daily activity pattern?

```r
stepsByIntervalAvg <- by(actData$steps, actData$interval,FUN=mean, na.rm=TRUE)
dfStepsByInterval <- data.frame(interval=unique(actData$interval), avg = as.vector(stepsByIntervalAvg))
plot(dfStepsByInterval$interval, dfStepsByInterval$avg, type="l" )
```

![](PA1_template_files/figure-html/averageDate-1.png) 

```r
maxAvgStep <- dfStepsByInterval[which.max(dfStepsByInterval$avg),1]
```

The 5-minute interval which gives the average across all the days in the dataset, containing the maximum number of steps is 835 

## Inputing missing values

Number of missing value records in the dataset is 2304


```r
actDataAvg <- merge(actData[which(is.na(actData)),], dfStepsByInterval, by= c("interval", "interval"))
actDataAvg$steps <- actDataAvg$avg
actData <- merge(actData, actDataAvg, by.x=c("date", "interval"), by.y = c("date", "interval"), all.x=TRUE)
names(actData)[3] <- "steps"
##replace missing values with average for the interval
actData <- within(actData, steps[is.na(actData$steps)] <- avg[is.na(actData$steps)] )
actData <- within(actData, rm(steps.y,avg))
actData <- actData[,c(3,1,2)]

stepsByDate <- as.vector(by(actData$steps, actData$date, FUN=sum))
hist(stepsByDate,
     col='red', 
     main = "Activity Data", 
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/missingValues-1.png) 

```r
meanStepsByDate <- round(mean(stepsByDate, na.rm=TRUE),2)
medianStepsByDate <- median(stepsByDate,na.rm=TRUE)
```

The mean of number of steps is 10766.19 and median is 10766.19
We can see that the mean remains the same since we used interval mean for filling the missing values but the median has gone up slightly because of the newly introduced set of values.



## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
 
actData <- cbind(actData, wFactor="")

actData$wFactor <- ifelse((weekdays(as.Date(actData$date)) %in% c("Saturday", "Sunday")), "Weekend", "Weekday")
actData$wFactor <- as.factor(actData$wFactor) 

a <- as.data.frame(as.list(aggregate(na.omit(actData), by=list(actData$interval, actData$wFactor), FUN= mean, na.rm=TRUE)))
names(a)[1] <- "stepInterval"
names(a)[2] <- "wFactor"
xyplot(a$steps ~ a$stepInterval| a$wFactor, type= "l", layout = c(1,2), xlab= "interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/weekdays-1.png) 


