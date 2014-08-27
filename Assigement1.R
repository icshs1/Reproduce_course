##Loading and preprocessing the data
rawdata<-read.csv("activity.csv")
##rawdata$steps[is.na(rawdata$steps)]<-0 ## Null steps transform to 0
  
##Total number of steps taken per day
Total.Steps<-tapply(rawdata$steps,rawdata$date,sum)
hist(Total.Steps,10,main="Total number of steps taken per day",xlab="Total Steps")
Total.Steps_mean<-mean(Total.Steps,na.rm=TRUE)
Total.Steps_median<-median(Total.Steps,na.rm=TRUE)
Total.Steps_mean
Total.Steps_median

##What is the average daily activity pattern?
refineData<-na.omit(rawdata)  
Pattern <- aggregate(refineData$steps, by=list(refineData$interval), FUN=mean)
plot(Pattern[,1],Pattern[,2],type="l",main="Average daily activity pattern",xlab="interval",ylab="average steps")
Max_interval<-Pattern[which.max(Pattern[,2]),1] ## interval contains the maximum number of steps
Max_interval
points(Max_interval,max(Pattern[,2]))
abline(v=Max_interval,col="red")

## the total number of missing values in the dataset 
sum(is.na(rawdata$steps))

## Compare the impact of imputing missing data between several methods

## first method is using the mean for that 5-minute interval
first_imputing<-rawdata
first_imputing$steps[is.na(first_imputing$steps)]<-Pattern[,2]
first_imputing.Steps<-tapply(first_imputing$steps,first_imputing$date,sum)
first_imputing.Steps_mean<-mean(first_imputing.Steps,na.rm=TRUE)
first_imputing.Steps_median<-median(first_imputing.Steps,na.rm=TRUE)
summary(first_imputing)

## second method is using kNN imputation 
install.packages("DMwR")
library(DMwR)
second_imputing<-knnImputation(rawdata)
second_imputing.Steps<-tapply(second_imputing$steps,second_imputing$date,sum)
second_imputing.Steps_mean<-mean(second_imputing.Steps,na.rm=TRUE)
second_imputing.Steps_median<-median(second_imputing.Steps,na.rm=TRUE)

par(mfrow=c(2,2))
hist(Total.Steps,10,main="Original Data",xlab="Total Steps")
hist(first_imputing.Steps,10,main="Data by imputing the mean",xlab="Total Steps")
hist(second_imputing.Steps,10,main="Data by kNN imputation ",xlab="Total Steps")

paste("The means of original data and impuing data are",Total.Steps_mean,",",first_imputing.Steps_mean,"and",second_imputing.Steps_mean)
paste("The medians original data and impuing data are",Total.Steps_median,",",first_imputing.Steps_median,"and",second_imputing.Steps_median)

## Activity patterns between weekdays and weekends

## Create a new factor variable in the dataset with two levels 
## ??? ¡°weekday¡± and ¡°weekend¡± 
Sys.setlocale("LC_TIME", "English")
week<-ifelse(weekdays(as.Date(second_imputing$date))=="Sunday"|weekdays(as.Date(second_imputing$date))=="Saturday","weekend","weekday")
week<-as.factor(week)
newdata<-cbind(second_imputing,week)

newPattern <- aggregate(newdata$steps, by=list(newdata$interval,newdata$week), FUN=mean)
colnames(newPattern)<-c("interval","week_type","activity")
library(ggplot2)
qplot(newPattern$interval,newPattern$activity,
      geom="line",color=newPattern$week_type,
      xlab="5 min interval",ylab="Number of Steps",main="#steps, split on weekend/weekdays")

weekend_data<-subset(newPattern,newPattern$week_type=="weekend")
weekday_data<-subset(newPattern,newPattern$week_type=="weekday")
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(weekend_data$interval,weekend_data$activity,type="l", xlab="", ylab="no of steps", main="weekend",col="blue")
title(main="#steps, split on weekend/weekdays", outer=TRUE)    
plot(weekday_data$interval,weekday_data$activity,type="l", xlab="5 min interval", ylab="no of steps", main="weekday",col="blue")




