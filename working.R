activity <- read.csv("activity.csv",header=TRUE, sep=",")


steps_day <- aggregate(steps ~ date, activity, sum)
#hist(steps_day[,c('steps')], col="lightblue", main="Histogram of Total Steps Per Day", xlab="Total Steps")

steps_day_mean <- mean(steps_day[,c('steps')])
steps_day_median <- median(steps_day[,c('steps')])


steps_interval_mean <- aggregate(steps ~ interval, activity, mean)

#plot(steps_interval_mean$interval,steps_interval_mean$steps, type="l", xlab="5-Minute Interval", ylab="Mean Number of Steps", main="Mean Number of Steps per 5-Minute Interval", col="blue")

activity2 <- activity
for(i in seq_along(activity2$steps)){
    row <- activity2[i,]
    if(is.na(row$steps)){
        activity2[i,]$steps <- steps_interval_mean[steps_interval_mean$interval==row$interval,]$steps
    }
}


activity2$weekday <- weekdays(as.POSIXct(strptime(activity2$date, "%Y-%m-%d")))
activity_weekday <- activity2[activity2$weekday=="Saturday" | activity2$weekday=="Sunday",]
activity_weekend <- activity2[activity2$weekday!="Saturday" & activity2$weekday!="Sunday",]
steps_interval_mean <- aggregate(steps ~ interval, activity, mean)

steps_interval_mean_weekday <- aggregate(steps ~ interval, activity_weekday, mean)
steps_interval_mean_weekend <- aggregate(steps ~ interval, activity_weekend, mean)
par(mfrow=c(2,1))
plot(steps_interval_mean_weekday$interval,steps_interval_mean_weekday$steps, type="l", xlab="5-Minute Interval Weekday", ylab="Mean Number of Steps", main="", col="blue")
plot(steps_interval_mean_weekend$interval,steps_interval_mean_weekend$steps, type="l", xlab="5-Minute Interval Weekend", ylab="Mean Number of Steps", main="", col="red")






