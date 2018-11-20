Reproducible Research_Course Project 1
Loading and preprocessing the data
activity <- read.csv("C:/Users/JJQ/Documents/data/activity.csv", header = TRUE, stringsAsFactors = FALSE)
summary(activity)
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
What is mean total number of steps taken per day?
library(dplyr)
## Warning: package 'dplyr' was built under R version 3.5.1
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
steps <- activity %>% select(steps, date) %>% group_by(date) %>% summarise(steps_per_day = sum(steps))

hist(steps$steps_per_day, main = "Total Number of Steps Taken per Day", xlab = "Steps Per Day", ylab = "Interval", col = "light blue")
 
mean_dailysteps <- mean(steps$steps_per_day, na.rm = TRUE)
mean_dailysteps
## [1] 10766.19
median_dailysteps <- median(steps$steps_per_day, na.rm = TRUE)
median_dailysteps
## [1] 10765
Answer: The mean of the total number of steps taken per day is equal to 10766.19 The median of the total number of steps taken per day is equal to 10765
What is the average daily activity pattern?
library(ggplot2)

average <- group_by(activity, interval) %>% summarise(average_steps = mean(steps, na.rm = TRUE))

ggplot(average, aes(x = interval, y = average_steps, type = "l")) + geom_line(color = "blue") + 
  ggtitle("Average Number of Steps by Interval") + xlab("Interval") + ylab("Average Steps across All Days")
 
interval_max_steps <- filter(average, average_steps == max(average$average_steps))
interval_max_steps
## # A tibble: 1 x 2
##   interval average_steps
##      <int>         <dbl>
## 1      835          206.
Answer: interval 835 contains the maximum number of steps. The person is very active at around 2:00pm (to be accurate at 1:55pm)
Imputig missing values
summary(is.na(activity))
##    steps            date          interval      
##  Mode :logical   Mode :logical   Mode :logical  
##  FALSE:15264     FALSE:17568     FALSE:17568    
##  TRUE :2304
activity[is.na(activity)] <- c(average$average_steps)
activity_new <- activity

steps_new <- activity_new %>% select(steps, date) %>% group_by(date) %>% summarise(steps_per_day_new = sum(steps))

hist(steps_new$steps_per_day_new, main = "Total Number of Steps Taken per Day (New Dataset)", xlab = "Steps Per Day (New)", ylab = "Interval", col = "blue")
 
mean_dailysteps_new <- mean(steps_new$steps_per_day_new, na.rm = TRUE)
mean_dailysteps_new
## [1] 10766.19
median_dailysteps_new <- median(steps_new$steps_per_day_new, na.rm = TRUE)
median_dailysteps_new
## [1] 10766.19
Answer: There are 2304 missing value in total in the dataset The mean of the total number of steps taken per day didn’t change after imputing, still equal to 10766.19 The median of the total number of steps taken per day increased slightly to 10766.19 from 10765, after imputing
Are there differences in activity patterns between weekdays and weekends
wkd <- weekdays(as.Date(strptime(activity_new$date, format =  "%Y-%m-%d")))
wkd <- as.factor(wkd)

activity_by_date <- cbind(activity_new, wkd)

levels(activity_by_date$wkd) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
                                     weekend = c("Saturday", "Sunday"))
levels(activity_by_date$wkd)
## [1] "weekday" "weekend"
par(mfrow = c(2,1))
with(activity_by_date[activity_by_date$wkd == "weekday", ], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Activity in Weekdays"))
with(activity_by_date[activity_by_date$wkd == "weekend", ], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Activity in Weekends"))
 
Answer: The person is more activate in weekends than in weekdays
