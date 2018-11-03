#Loading
        measurements <- read.csv(unz("activity.zip", "activity.csv"))

##What is mean total number of steps taken per day?
##Plot histogram
        stepsPerDay <- aggregate(steps ~ date, measurements, sum,na.rm=TRUE)
        hist(stepsPerDay$steps, main = "Steps per day with NAs", xlab = "Steps", col = "blue", breaks = 10)

        #calculate median steps per day (10765)
        medianSteps <- median(stepsPerDay$steps)
        
        #calculate mean steps per day(10766.19)
        meanSteps <- mean(stepsPerDay$steps)

#What is the average daily activity pattern?
        stepsInterval <- aggregate(steps ~ interval, measurements, mean,na.rm=TRUE)
        plot(stepsInterval$interval, stepsInterval$steps, type="l", xlab = "5 minute interval", ylab = "Average steps", main = "Average Daily Activity Pattern", col = "blue")
        
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        stepsInterval$interval[which.max(stepsInterval$steps)]
        
# How many missing values?
        nrow(measurements[is.na(measurements$steps),])
        
# Set missing values to zero.
        measurementsSansNAs <- measurements
        measurementsSansNAs[is.na(measurementsSansNAs$steps), "steps"] <-0
        
#Histogram again with no NAs
        stepsPerDaySansNAs <- aggregate(steps ~ date, measurementsSansNAs, sum)
        hist(stepsPerDaySansNAs$steps, main = "Steps per day", xlab = "Steps", col = "blue", breaks = 10)
        
#Median again with no NAs (10395)
        medianStepsSansNAs <- median(stepsPerDaySansNAs$steps)
#Mean with zeros (9354.23)
        meanStepsSansNAs <- mean(stepsPerDaySansNAs$steps)
        
#Are there differences in activity patterns Weekday vs Weekend?
        measurementsSansNAs$day <- as.POSIXlt(measurementsSansNAs$date)$wday
        measurementsSansNAs$dayType <- as.factor(ifelse(measurementsSansNAs$day == 6 | measurementsSansNAs$day == 0, "Weekend", "Weekday"))
        measurementsSansNAs <- subset(measurementsSansNAs, select = -c(day))

#Have a looksy
        head(measurementsSansNAs, n=50)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all weekday days or weekend days (y-axis).
        weekdaysData <- measurementsSansNAs[measurementsSansNAs$dayType == "Weekday",]
        weekendsData <- measurementsSansNAs[measurementsSansNAs$dayType == "Weekend",]
        stepsIntervalWeekdays <- aggregate(steps ~ interval, weekdaysData, mean)
        stepsIntervalWeekends <- aggregate(steps ~ interval, weekendsData, mean)
        
        par(mfrow = c(2, 1))
        
        plot(stepsIntervalWeekdays, type = "l", col = "blue", main = "Weekdays")
        plot(stepsIntervalWeekends, type = "l", col = "red", main = "Weekends")
        