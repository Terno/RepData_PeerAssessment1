# Week 2 - Peer-graded assignment

## Step 1 - Loading and preprocessing the data
```{r}
#read data and ste date format
activity<-read.csv("activity.csv",header=TRUE, stringsAsFactors = FALSE)
activity$date<-as.POSIXct(activity$date)
```

## Step 2 - Determine mean total number of steps taken per day (NA's Included)
```{r}

dailySteps<-aggregate(steps~date, data=activity, FUN = sum)

hist(dailySteps$steps, main = "Frequency of Steps per Day", xlab = "Steps per Day")

print("Median steps per day")
median(dailySteps$steps)
print("Mean steps per day")
mean(dailySteps$steps)

```
![](figures/dailyStepsHist_NI.jpeg)<!-- -->


## Step 3 - Determine daily activity patterns (NA's Included)
```{r}
intervals<-aggregate(steps~interval, data=activity, 
        FUN = mean)

plot(intervals$steps, type = "l", main = "Avg. steps per time interval",
     xlab = "5-minute time interval", ylab = "Avg. steps")
```
![](figures/avgStepsInterval_NI.jpeg)<!-- -->

## Step 4 - Impute missing values
```{r}
print("Empty rows")
nrow(activity)-nrow(na.omit(activity))

for(i in 1:nrow(activity)){
        if(is.na(activity[i,"steps"])){
                activity[i,"steps"]<-intervals$steps[intervals$interval==activity[i,"interval"]]
        }
}

dailySteps<-aggregate(steps~date, data=activity, FUN = sum)

hist(dailySteps$steps, main = "Frequency of Steps per Day - Imputed NA's", xlab = "Steps per Day")

print("Median steps per day - Imputed NA's")
median(dailySteps$steps)
print("Mean steps per day - Imputed NA's")
mean(dailySteps$steps)

```
![](figures/dailyStepsHist_I.jpeg)<!-- -->

## Step 5 - Compare weekday and weekend activity patterns - NA's Imputed
```{r}
activity$dow<-weekdays(activity$date)

weekdays<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$wDay <- factor(((activity$dow) %in% weekdays), levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))

intervals<-aggregate(steps~interval+wDay, data=activity, 
        FUN = mean)

par(mfrow=c(2,1))
plot(intervals$steps[intervals$wDay=="Weekday"], type = "l",main="Weekday",
     ylab="Steps", xlim = c(min(intervals$steps),max(intervals$steps)))
plot(intervals$steps[intervals$wDay=="Weekend"], type = "l",main="Weekend",
     ylab="Steps", xlim = c(min(intervals$steps),max(intervals$steps)))
```

![](figures/DOWComparison.jpeg)<!-- -->
