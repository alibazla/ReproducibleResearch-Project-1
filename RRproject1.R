library(lubridate)
library(ggplot2)
activity <- read.csv("activity.csv")
dim(activity)
head(activity)
names(activity)
str(activity)
length(unique(activity$date))
##
activity_total_steps <- with(activity, aggregate(steps, by= list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
hist(activity_total_steps$steps, main = "Total number of steps per day", xlab = "Number of steps", col = "red", ylim = c(0, 20))
png("plot1.png")
##
mean(activity_total_steps$steps)
##median
median(activity_total_steps$steps)

##Daily Activity
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm = TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col = "red", lwd = 2, xlab = "Interval", ylab = "Number of steps", main = "Average Number of Steps per Interval" )
average_daily_activity[which.max(average_daily_activity$mean), ]$interval

##Imputing Missing Values
sum(is.na(activity$steps))
###filling in all the missing values
imputed_steps <- average_daily_activity$mean [ match(activity$interval, average_daily_activity$interval)]
###Creating new datasets
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
###Create Histogram
hist(total_steps_imputed$daily_steps, col = "red", xlab = "Number of Steps per Day", ylim = c(0, 30), main = "Total Number of Steps per Day")
###mean of the total numberof steps taken per day
mean(total_steps_imputed$daily_steps)

###median of the total number of steps taken per day
median(total_steps_imputed$daily_steps)

## Difference in activity patterns between weekday and weekend
library(lattice)
activity$date <- as.Date(strptime(activity$date, format = "%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"} else {y <- "Weekday"} 
  y})
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, type = "l", layout = c(1,2), na.rm = TRUE)
plot <- ggplot(activity_by_date, aes(x = "interval", y = "steps", color = datetype)) + geom_line() + labs(title="Average Daily Steps by Type of Date",x="Interval", y="Number of Steps") +facet_wrap(~datetype, ncol = 1, nrow = 2)
print(plot)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
