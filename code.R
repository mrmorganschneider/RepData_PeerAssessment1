library(tidyverse)

#part 1
unzip("activity.zip")
raw_data <- read.csv("activity.csv")

#part 2
summed_data <- with(raw_data, aggregate(steps, list(date), FUN=sum))
hist(summed_data$x, main = "Number of steps per day", 
    xlab = "Number of steps", ylab = "Number of days")

mean(summed_data$x, na.rm = TRUE)

median(summed_data$x, na.rm = TRUE)

#part 3
avg_data <- with(na.omit(raw_data), 
    aggregate(steps, list(interval), FUN = mean))
plot(avg_data, type ="l", main="Average number of steps per interval",
    xlab = "Interval", ylab = "Average number of steps")

avg_data[which.max(avg_data$x),]

#part 4
sum(is.na(raw_data$steps))

new_data <- cbind(raw_data, avg_data$x)
names(new_data)[4] <- "average"

for( i in 1:length(new_data$steps)){
    if(is.na(new_data[i,]$steps)){
        new_data[i,]$steps <- new_data[i,]$average
    }
}

#part 5

new_data$day <- weekdays(as.Date(new_data$date))
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_data$wDay <- factor((new_data$day %in% weekdays), levels=c(FALSE, TRUE), 
    labels=c('weekend', 'weekday'))

weekdayData <- with(subset(new_data, wDay == 'weekday'), 
    aggregate(steps, list(interval), FUN = mean))
weekdayData$wDay <- ('weekday')
names(weekdayData) <- c('Interval', 'Steps', 'wDay')

weekendData <- with(subset(new_data, wDay == 'weekend'), 
    aggregate(steps, list(interval), FUN = mean))
weekendData$wDay <- ('weekend')
names(weekendData) <- c('Interval', 'Steps', 'wDay')

total_data <- rbind(weekendData, weekdayData)

p <- ggplot(data = total_data, aes(x = Interval, y = Steps)) + geom_line()
p <- p + facet_wrap(~wDay, ncol = 1)
p + labs(x = "Interval", y = "Average number of steps")
