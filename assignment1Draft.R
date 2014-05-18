# Assignment Number 1 for Reproducible Research
setwd("C:/Users/UER/Documents/GitHub/RepResearch_PeerAssignment1")


dev.off() # this is going to work only if I have a device previously opened
# First Part
# 1. Load the data (i.e. read.csv())
# Reading data from file
data <- read.csv("activity.csv")

# 2. Process/transform the data (if necessary) into a format suitable for your 
# analysis
data$date <- as.Date(data$date, "%Y-%m-%d")

# Cleaning the data from na values
cleanData <- subset(data,steps!="NA")

#  Previous Analysis
# head(cleanData)
# tail(cleanData)
# summary(cleanData)



# Second Part
# What is mean total number of steps taken per day?

# 2. Calculate and report the mean and median total number of steps taken per day

# getting the means   
mean_by_date <- tapply(cleanData$steps, cleanData$date, mean)
total_steps_by_date <- tapply(cleanData$steps, cleanData$date, sum)

# 1. Make a histogram of the total number of steps taken each day
# hist(mean_by_date, col="red", main = "Means Histogram",xlab = "Days") # graph to find the histogram 
hist(total_steps_by_date, col="red", main = "Total Steps frecuency Histogram",xlab = "") # graph to find the histogram 


# getting the medians
median_by_date <- tapply(cleanData$steps, cleanData$date,median)
titles <- c("day","mean","median")
Report1 <- cbind(names(mean_by_date),as.vector(mean_by_date),as.vector(median_by_date))
colnames(Report1) <- titles
head(Report1) ; tail(Report1)
Report1 <- as.data.frame(Report1)
Report1$day <- as.Date(Report1$day, "%Y-%m-%d")
Report1$mean <- mean_by_date
Report1$median <- median_by_date 

Report1 # This is to show the table with the mean and median for day

# Third Part 
# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
mean_by_interval <- tapply(cleanData$steps, cleanData$date, mean)

plot(as.Date(names(mean_by_interval)),as.vector(mean_by_interval), type = "l" , main = "5-minute interval Vs average number of steps taken", xlab = "5-minute interval" , ylab="Mean by Interval" , col = 4)

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

names(which(mean_by_interval==max(mean_by_interval)))


# Fourth Part
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(data$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


# Create a new dataset that is equal to the original dataset but with the missing data filled in.

naIndices <- which(is.na(data$steps))
dataCopy <- data # Copio el set de datos original
naIntervalsTreated <- dataCopy$interval[naIndices]
dataCopy$steps <- ifelse(is.na(dataCopy$steps),mean_by_interval[naIntervalsTreated],
                        dataCopy$steps) # I'm generating a copy with the NA's filled


dataCopy$steps[naIndices] <- mean_by_interval
(filledMedianDate <- tapply(dataCopy$steps, dataCopy$date,median))
(filledMeanInterval <- tapply(dataCopy$steps, dataCopy$date, mean))

hist(filledMeanInterval, col="blue", main = "Means Histogram recalculated",xlab = "Days") # graph to find the histogram 
# 
par(mfcol=c(2,2))
hist(total_steps_by_date, col="red", main = "Total Steps histogram",xlab = "Days") 
# graph to find the histogram 
hist(filledMeanInterval, col="cyan", main = "Filled Means Histogram",
     xlab = "Days") # graph to find the histogram 
hist(median_by_date,  main = "Original Median Histogram",xlab = "Days") # graph to find the histogram 
hist(filledMedianDate, col="blue", main = "Filled Median Histogram recalculated",
     xlab = "Days") # graph to find the histogram 


# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# This part is intented to distinguish between weekdays and weekends
# Please take Weekends as Saturdays and sundays

weekendVector <- weekdays(dataCopy$date) 
weekendVector2 <- weekendVector

daysIndexes <- c("lunes"=1, "martes"=2, "miércoles"=3, "jueves"=4, 
                                                       "viernes"=5, "sábado"=6, "domingo"=7)

weekendVector2<-daysIndexes[weekendVector]
weekendVector3 <- as.factor(ifelse(weekendVector2<6,"weekday","weekend"))
dataCopy_with_weekends <- cbind(dataCopy,weekendVector3)
# head(dataCopy_with_weekends)

# using subset function 
newdata_weekdays <- subset(dataCopy, weekendVector3 == "weekday")
newdata_weekends <- subset(dataCopy, weekendVector3 == "weekend")

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


# Usando datacopywithweekends hacer el promedio de los pasos (steps) por intervalo
mean_steps_by_interval_weekdays <- tapply(newdata_weekdays$steps, newdata_weekdays$interval, mean)
mean_steps_by_interval_weekends <- tapply(newdata_weekends$steps, newdata_weekends$interval, mean)
# I have to remember that this is just a 2 pieces vector, date and mean

dev.off() #Again, just to close the previous device
# 
par(mfrow=c(2,1))
plot(as.numeric(names(mean_steps_by_interval_weekdays)),mean_steps_by_interval_weekdays,type="l",col=2,main="Mean Steps by Interval",
     ylab="weekday Steps",xlab="Interval")
plot(as.numeric(names(mean_steps_by_interval_weekends)),mean_steps_by_interval_weekends,type="l",col=3,
     ylab="weekend steps",xlab="Interval")
