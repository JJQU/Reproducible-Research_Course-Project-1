library(dplyr)
library(ggplot2)
library(datasets)

if(!file.exists("data")){
  dir.create("data")
}
list.files("./data")

#Loading and preprocessing the data into a suitable format
#Group data by date to calculate the total number of steps taken per day
#Make a histogram of the total number of steps taken per day using base plotting system
#Calculate the mean and median of the total number of steps taken per day

activity <- read.csv("./data/activity.csv", header = TRUE, stringsAsFactors = FALSE)
str(activity)
head(activity)
tail(activity)

steps <- activity %>% select(steps, date) %>% group_by(date) %>% summarise(steps_per_day = sum(steps))
str(steps)
head(steps)
tail(steps)

hist(steps$steps_per_day, main = "Total Number of Steps Taken per Day", xlab = "Steps Per Day", ylab = "Interval", col = "light blue")

mean_dailysteps <- mean(steps$steps_per_day, na.rm = TRUE)
mean_dailysteps

median_dailysteps <- median(steps$steps_per_day, na.rm = TRUE)
median_dailysteps

#Answer: The mean of the total number of steps taken per day is equal to 10766.19
#Answer: The median of the total number of steps taken per day is equal to 10765

#Look for the average daily activity pattern
#Group data by interval to calcualte the average number of steps taken at each interval across all of 61 days
#Look for the 5-minutes interval that contains the maximum number of steps
average <- group_by(activity, interval) %>% summarise(average_steps = mean(steps, na.rm = TRUE))
str(average)
head(average)
tail(average)

ggplot(average, aes(x = interval, y = average_steps, type = "l")) + geom_line(color = "blue") + 
  ggtitle("Average Number of Steps by Interval") + xlab("Interval") + ylab("Average Steps across All Days")

interval_max_steps <- filter(average, average_steps == max(average$average_steps))
interval_max_steps

#Answer: interval 835 contains the maximum number of steps. The person is very active at around 2:00pm (to be accurate at 1:55pm)


#Imputing missing values
#Calculate the total number of rows that have missing value
#It is more reasonable to replace missing value with the mean of that 5-minutes interval.
#For example, on 2012-10-01, there is missing vale at the second 5 interval. Replace it with 0.339, the mean of that 5-minutes interval across all day.
summary(is.na(activity))


activity[is.na(activity)] <- c(average$average_steps)
activity_new <- activity
str(activity_new)
sthead(activity_new)
tail(activity_new)

steps_new <- activity_new %>% select(steps, date) %>% group_by(date) %>% summarise(steps_per_day_new = sum(steps))

hist(steps_new$steps_per_day_new, main = "Total Number of Steps Taken per Day (New Dataset)", xlab = "Steps Per Day (New)", ylab = "Interval", col = "blue")

mean_dailysteps_new <- mean(steps_new$steps_per_day_new, na.rm = TRUE)
mean_dailysteps_new

median_dailysteps_new <- median(steps_new$steps_per_day_new, na.rm = TRUE)
median_dailysteps_new

#Answer: There are 2304 missing value in total in the dataset
#Answer: The mean of the total number of steps taken per day didn't change after imputing, still equal to 10766.19
#Answer: The median of the total number of steps taken per day increased slightly to 10766.19 from 10765, after imputing

# Are there differences in activity patters between weekdays and weekends?
wkd <- weekdays(as.Date(strptime(activity_new$date, format =  "%Y-%m-%d")))
wkd <- as.factor(wkd)
str(wkd)

activity_by_date <- cbind(activity_new, wkd)
head(activity_by_date)
str(activity_by_date)

levels(activity_by_date$wkd) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
                                     weekend = c("Saturday", "Sunday"))
levels(activity_by_date$wkd)

par(mfrow = c(2,1))
with(activity_by_date[activity_by_date$wkd == "weekday", ], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Activity in Weekdays"))
with(activity_by_date[activity_by_date$wkd == "weekend", ], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Activity in Weekends"))


