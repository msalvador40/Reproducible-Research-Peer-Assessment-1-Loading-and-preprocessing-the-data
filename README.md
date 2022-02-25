# Reproducible-Research-Peer-Assessment-1-Loading-and-preprocessing-the-data
Reproducible Research: Peer Assessment 1 Loading and preprocessing the data


### Load packages

library(ggplot2)
library(readr)
library(lubridate)
library(tibble)
library(tidyverse)



### read dataset


activity <- readr::read_csv("C:/Users/Marcelo/Documents/R/activity.csv")


activity <- activity %>% mutate(date = as.Date(date, format = "%Y-%m-%d" )) %>%
  mutate(weekday = weekdays(date))



# chage col string for data


activity %>% summary()


##  1. What is mean total number of steps taken per day?

library(ggplot2)

activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
hist(activity_total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))


mean(activity_total_steps$steps)



median(activity_total_steps$steps)


write.csv(activity, "C:/Users/Marcelo/Documents/R/activity.csv")


### 2. What is the average daily activity pattern?

average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")

activity %>% group_by(interval) %>% 
  summarise(media = mean(steps, na.rm=T)) %>% 
  ggplot() + 
  geom_line(aes(x=interval, y=media))


activity %>% group_by(interval) %>% 
  summarise(media = mean(steps, na.rm=T)) %>% 
  arrange(desc(media))

### measures and data trasnformation

sum(is.na(activity$steps))

average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")

steps_na <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]

activity_na <- transform(activity, steps = ifelse(is.na(activity$steps), yes = steps_na, no = activity$steps))

steps_na_total <- aggregate(steps ~ date, activity_na, sum)

names(steps_na_total) <- c("date", "daily_steps")

hist(steps_na_total$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

steps_na_total %>% ggplot()+
  geom_histogram(aes(daily_steps), color = "black", fill = "blue", binwidth = 2000)

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "sábado" | weekdays(x) =="domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })




activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)

#### ALL PLOTS


![1](https://user-images.githubusercontent.com/96654326/155806656-2e116aff-c86c-43f9-81b9-69f170a02b2a.png)
![2](https://user-images.githubusercontent.com/96654326/155806662-12dc5443-4a7b-4fe7-9748-84bc45162a38.png)
![3](https://user-images.githubusercontent.com/96654326/155806663-5cf227d6-33ce-45c3-8785-1e87cc068e1b.png)
![4](https://user-images.githubusercontent.com/96654326/155806665-4ca83640-2b1d-47ad-9a40-73ded141bef3.png)
![5](https://user-images.githubusercontent.com/96654326/155806667-06ba39af-5a76-4564-933a-390a8ba6d02a.png)
![6](https://user-images.githubusercontent.com/96654326/155806672-bff39c1c-8836-4ea7-81b0-4ad7772caf9b.png)
![7](https://user-images.githubusercontent.com/96654326/155806674-d7c40090-af1d-4872-8219-e65687b9a33b.png)



