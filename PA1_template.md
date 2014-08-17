# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
```

## Loading and preprocessing the data


```r
filePath <- paste0(getwd(),"/activity.zip")

datset <- read.csv(unz(filePath,"activity.csv"))
```

## What is mean total number of steps taken per day?

### For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day



### 1.

```r
totalsum <- with(datset,aggregate(steps,by=list(date),FUN = sum, na.rm=FALSE))
colnames(totalsum) <- c("date","steps")

plot <- ggplot(data=totalsum,aes(x=date, y=steps)) +
        geom_histogram(stat="identity",fill="darkblue") +  
        scale_y_continuous(breaks = seq(0,25000,2000)) +   
        theme(axis.text.x = element_text(size= 8.5,angle = 45, 
                                         hjust = 1,colour="darkred",face="bold")) +
        theme(axis.text.y = element_text(size= 8.5, 
                                         colour="darkred",face="bold")) +
        ggtitle("The total number of steps taken each day.") +
        theme(plot.title=element_text(size = 20,face="bold"))


suppressWarnings(print(plot))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


### 2.

```r
mean <- mean(totalsum$steps,na.rm=TRUE)
median <-median(totalsum$steps,na.rm=TRUE)
```

The mean for total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> .

The median for total number of steps taken per day is 10765 .
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


### 1.

```r
meanInt <- with(datset,aggregate(steps,by=list(interval),FUN= mean,na.rm=TRUE))
colnames(meanInt) <- c("interval","steps")
plot <- ggplot(data=meanInt,aes(x=interval,y=steps)) + 
        geom_line(col="red",lwd=0.9) +
        scale_x_continuous(breaks = seq(0,2355,150)) + 
        scale_y_continuous(breaks = seq(0,205,50)) +
        theme(axis.text.x=element_text(size=10,colour="blue",
                                       face="bold")) +
        theme(axis.text.y=element_text(colour="blue",
                                       face="bold")) +
        ggtitle("The average number of steps by 5-minute intervals across all the days .") +
        theme(plot.title=element_text(size = 12,face="bold"))
print(plot)     
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

### 2.

```r
maximus <- meanInt[which(meanInt$steps == max(meanInt$steps)),] 
maximus
```

```
##     interval steps
## 104      835 206.2
```
## Imputing missing values

**NOTE: There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.**

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

### 1.

```r
totalNA <- table(complete.cases(datset))
names(totalNA) <- c("NArows","nonNArows")
totalNA
```

```
##    NArows nonNArows 
##      2304     15264
```
### 2.

**The strategy devised for filling in all of the missing values in the dataset consists on identifying the dates (YYYY-MM-DD) when/where steps data is Not Available (Assuming steps as the only column with missing values coded as NA).**

**Dates with NA are reformatted to YYYY-DD in order to verify, compare and fill steps values based on the mean calculated from same days (%d) during the same year with no missing values.** 

**Example, if date "2012-11-30" contains missing value, then it may be filled with the steps mean calculated from same day(s) from different months of the year, in this case filled from "2012-10-30" mean calculations. If all same days (for all months) contain NA or no monthly equivalent day(s) are found, steps is assigned zero.**   


### 3.

```r
datset2 <- datset
tempset <- datset2 
## Change date format to "YYYY-DD" in temporary data set.
tempset$date <- format(as.Date(tempset$date),"%Y-%d")
## ID rows with missing values in temporary data set.
narow <- !complete.cases(tempset)
## ID unique dates with missing values in temporary data set.
nadates <- unique(tempset[narow,"date"])

for (i in nadates) {
        ## ID indexes of all rows for a specific date(s) of format "YYYY-DD".           
        idx <- grep(i,tempset[,2])
        
        ## If all same days (for all months) contain NA or no monthly 
        ## equivalent day(s) are found, ##steps is assigned zero.
        if (all(is.na(tempset[idx,1]))) {
                
                
                 datset2[idx,1] <- 0
                
        } else {
                        
                ## Else calculate mean from other months (but same day) and assign it to steps columns with NA.       
                  mean <- mean(as.vector(tempset[idx,1]),na.rm=TRUE)
                  idx2 <- as.integer(which(is.na(tempset[idx,1]), arr.ind=TRUE))
                  datset2[idx[idx2],1] <- mean
                        
                }
        
        
        }
```
### 4.

```r
totalsum <- with(datset2,aggregate(steps,by=list(date),FUN = sum, na.rm=FALSE))
colnames(totalsum) <- c("date","steps")

plot <- ggplot(data=totalsum,aes(x=date, y=steps)) +
        geom_histogram(stat="identity",fill="darkblue") +  
        scale_y_continuous(breaks = seq(0,25000,2000)) + 
        theme(axis.text.x = element_text(size= 8.5,angle = 45, 
                                         hjust = 1,colour="darkred",face="bold")) +
        theme(axis.text.y = element_text(size= 8.5, 
                                         colour="darkred",face="bold")) +
        ggtitle("The total number of steps taken each day.") +
        theme(plot.title=element_text(size = 20,face="bold"))

print(plot)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean <- mean(totalsum$steps,na.rm=TRUE)
median <-median(totalsum$steps,na.rm=TRUE)


mean <- cbind(mean,median)
kable(mean,format="markdown")
```

```
## 
## 
## |  mean| median|
## |-----:|------:|
## | 10386|  10600|
```

**Yes, the mean/median calculations, for total number of steps taken per day, differ from the estimates from the first part of the assignment. This may be in part because of the inclusion/filling strategy used on NA values.**

**In this case, the impact of imputing missing values seems minimal. In the first part of the assignment we ignored
NA values (na.rm=T), if not the results would be compromised with NA outputs. In this part by including/filling in the NA values with estimates the mean/median values changed, but not dramatically and we were able to include all dates under the two
month period** 

## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

### 1.

```r
datset2$weekdays <- as.factor(ifelse(weekdays(as.Date(datset2$date)) 
                                     %in% c("Saturday","Sunday"),"Weekend","Weekday"))
```

### 2.

```r
mean <- with(datset2,aggregate(steps,by=list(interval,weekdays),FUN = mean, na.rm=FALSE))
colnames(mean) <- c("interval","weekdays","steps")

plot <-ggplot(data=mean, aes(x =interval,y =steps)) +
        geom_line(col="red",lwd=0.9) +
        facet_wrap( ~ weekdays, nrow=2,ncol=1) +
        scale_x_continuous(breaks = seq(0,2355,150)) + 
        scale_y_continuous(breaks = seq(0,205,50)) +
        theme(axis.text.x=element_text(size=10,colour="blue",
                                       angle=45,face="bold")) +
        theme(axis.text.y=element_text(colour="blue",
                                       face="bold")) +
        ggtitle("The average number of steps by 5-minute intervals during weekdays/weekends.") +
        theme(plot.title=element_text(size = 12,face="bold"))
print(plot)              
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
