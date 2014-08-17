# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data
### The file "activity.csv" is downloaded to the home Documents Folder

```r
setwd("~/Documents")
Activity<-read.csv("activity.csv",header=TRUE,comment.char = "",colClasses=
                         c("integer","character","integer"),na.strings='NA')
Activity$date<-as.Date(Activity$date,"%Y-%m-%d")
```
## What is mean total number of steps taken per day?
### Histogram of steps per day

```r
library(reshape2)
ActivityMelt<-melt(Activity,id.vars=c("date","interval"),na.rm=TRUE)
ActivityTotal<-dcast(ActivityMelt,date~variable,fun=sum)
hist(ActivityTotal$steps,main="Total Number of Steps Taken Each Day",
     xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Mean and Median Total Steps Each Day

```r
ActivityDateMean<-mean(ActivityTotal$steps,na.rm=TRUE)
ActivityDateMedian<-median(ActivityTotal$steps,na.rm=TRUE)
Report<-cbind(ActivityDateMean,ActivityDateMedian)
colnames(Report)<-c("mean steps","median steps")
library(xtable)
print(xtable(Report),type="html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun Aug 17 11:23:52 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> mean steps </TH> <TH> median steps </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10765.00 </TD> </TR>
   </TABLE>
## What is the average daily activity pattern?
### Time Series of Average Steps taken by time interval

```r
ActivityIntervalMean<-dcast(ActivityMelt,interval~variable,fun=mean)
plot(ActivityIntervalMean$interval,ActivityIntervalMean$steps,type="l",
     main="Average Daily Steps By Time Interval",xlab="Time Interval",
     ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### Which 5-minute interval contains the maximum number of steps?

```r
ActivityIntervalMean[ActivityIntervalMean$steps==
                             max(ActivityIntervalMean$steps),1]
```

```
## [1] 835
```
## Imputing missing values
###Calculate and report the total number of missing values in the dataset

```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```
### Replace NA's with mean of interval

```r
Activity$steps2<-sapply(seq_len(nrow(Activity)),function (x) 
        ifelse(is.na(Activity$steps[[x]]),
               ActivityIntervalMean[ActivityIntervalMean$interval== 
                                            Activity$interval[[x]],2],
               Activity$steps[[x]]))
```
### Histogram of steps per day

```r
ActivityMelt<-melt(Activity,id.vars=c("date","interval"),na.rm=FALSE)
ActivityTotal<-dcast(ActivityMelt,date~variable,fun=sum)
par(mfrow=c(1,2))
hist(ActivityTotal$steps,main="Total Steps Taken Each Day",
     sub="Ignore NA",ylim=c(0,37),
     xlab="Number of Steps")
hist(ActivityTotal$steps2,main="Total Steps Taken Each Day",
     sub="Estimate NA From Interval Mean",ylim=c(0,37),
     xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### Mean and Median Steps Each Day

```r
ActivityDateMean<-mean(ActivityTotal$steps,na.rm=TRUE)
ActivityDateMean2<-mean(ActivityTotal$steps2)
ActivityDateMedian<-median(ActivityTotal$steps,na.rm=TRUE)
ActivityDateMedian2<-median(ActivityTotal$steps2)
Report<-cbind(ActivityDateMean,ActivityDateMean2, ActivityDateMedian,
              ActivityDateMedian2)
colnames(Report)<-c("mean steps - ignore NA",
                    "mean steps - estimate NA based on mean for the interval",
                    "median steps - ignore NA",
                    "median steps - estimate NA based on mean for the interval")
print(xtable(Report),type="html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun Aug 17 11:23:52 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> mean steps - ignore NA </TH> <TH> mean steps - estimate NA based on mean for the interval </TH> <TH> median steps - ignore NA </TH> <TH> median steps - estimate NA based on mean for the interval </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10765.00 </TD> <TD align="right"> 10766.19 </TD> </TR>
   </TABLE>
### The NA values are for full days.  The result of replacing the NA values with 
### the mean for each interval is to simply create more incidences of an average
### day.  An average day has 10766.19 steps.  So the median becomes the mean.
## Are there differences in activity patterns between weekdays and weekends?

```r
Activity$weekend<-ifelse(weekdays(Activity$date)=="Saturday"|
                                 weekdays(Activity$date)=="Sunday",
                         "weekend","weekday")
Activity$weekend<-as.factor(Activity$weekend)
ActivityMelt<-melt(Activity,id.vars=c("date","interval","weekend"),na.rm=TRUE)
ActivityIntervalMean<-dcast(ActivityMelt,interval+weekend~variable,fun=mean)
library(lattice)
xyplot(steps~interval|weekend,data=ActivityIntervalMean, 
       main="Average Daily Steps By Time Interval",layout=c(1,2),type="l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
