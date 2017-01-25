activity  = read.csv("activity.csv")
meanSteps = activity %>% group_by(date) %>% summarise(avg=mean(steps,na.rm = TRUE))
medianSteps = activity %>% group_by(date) %>% summarise(avg=median(steps,na.rm = TRUE))
countSteps = activity %>% group_by(date) %>% summarise(sumSteps=sum(steps))
hist(countSteps$sumSteps, col="Blue", main="Histogram of Average Step Count", xlab="Average # of Steps per Day" )

avg5min = activity %>% group_by(interval) %>% summarise(avg=mean(steps,na.rm = TRUE))
plot(x=avg5min$interval, 
     y=avg5min$avg, 
     type="l", 
     main="Average steps taken through the day", 
     xlab="5-minute intervals", ylab="Average # of Steps")

length(activity$interval[is.na(activity$steps)])

imputed = activity %>% left_join(avg5min, by="interval") %>% mutate(steps = ifelse(test = is.na(steps),avg,steps)) %>% select(steps,date,interval)
meanStepsImputed = imputed %>% group_by(date) %>% summarise(avg=mean(steps))
medianStepsImputed = imputed %>% group_by(date) %>% summarise(avg=median(steps))
countStepsImputed = imputed %>% group_by(date) %>% summarise(sumSteps=sum(steps))
hist(countStepsImputed$sumSteps, col="Blue", main="Histogram of Average Step Count With NAs Imputed", xlab="Average # of Steps per Day" )


imputed$date = as.POSIXlt(imputed$date)
imputed$daytype = ifelse(imputed$date$wday %in% c(0,6), "weekend","weekday")
imputed$daytype = factor(imputed$daytype )

imputed$date = factor(imputed$date)
avg5min = imputed %>% group_by(interval,date) %>% summarise(avg=mean(steps,na.rm = TRUE))
plot(x=avg5min$interval, 
     y=avg5min$avg, 
     type="l", 
     main="Average steps taken through the day", 
     xlab="5-minute intervals", ylab="Average # of Steps")

library("ggplot2")
imputed$date = as.character(imputed$date)
imputed$date = factor(imputed$date)
avgStepsPerIntervalPerDayType = imputed %>% group_by(interval,daytype) %>% summarise(avg=mean(steps))
qplot(data=avgStepsPerIntervalPerDayType,x = interval, y=avg, facets=.~daytype, geom = "line")