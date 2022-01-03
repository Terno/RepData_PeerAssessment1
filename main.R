#read data and prep data

activity<-read.csv("activity.csv",header=TRUE, stringsAsFactors = FALSE)
activity$date<-as.POSIXct(activity$date)


### What is mean total number of steps taken per day?
dailySteps<-aggregate(steps~date, data=activity, FUN = sum)

hist(dailySteps$steps)

dailyStepsMedian<-median(dailySteps$steps)
dailyStepsMean<-mean(dailySteps$steps)
dailyStepsTotal<-sum(dailySteps$steps)

### What is the average daily activity pattern?
intervals<-aggregate(steps~interval, data=activityClean, 
        FUN = mean)
plot(intervals$steps, type = "l")


### Imputing missing values

emptyRows<-nrow(activity)-nrow(na.omit(activity))

for(i in 1:nrow(activity)){
        if(is.na(activity[i,"steps"])){
                activity[i,"steps"]<-intervals$steps[intervals$interval==activity[i,"interval"]]
        }
}

dailySteps<-aggregate(steps~date, data=activity, FUN = sum)

hist(dailySteps$steps)

dailyStepsMedianNARM<-median(dailySteps$steps)
dailyStepsMeanNARM<-mean(dailySteps$steps)
dailyStepsTotalNARM<-sum(dailySteps$steps)

### Are there differences in activity patterns between weekdays and weekends?

activity$dow<-weekdays(activity$date)

weekdays<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$wDay <- factor(((activity$dow) %in% weekdays), levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))

par(mfrow=c(2,1))
plot(activity$steps[activity$wDay=="weekday"], type = "l",main="Weekday",
     ylab="Steps")
plot(activity$steps[activity$wDay=="weekend"], type = "l",main="Weekend",
     ylab="Steps")
