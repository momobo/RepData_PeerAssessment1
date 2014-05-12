# Raw R code for peer assessment 1 of reproducible research
# not really necessary
Url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

wd="C:\\Users\\mmorelli\\Google Drive\\Data Science\\05 - Reproducible Research\\01 Peer\\RepData_PeerAssessment1"
setwd(wd)

getwd()


# powCons <- read.csv(unzip("../../data/hpc.zip"), sep=";", stringsAsFactors=F)
activ <- read.csv(unzip("./activity.zip"), stringsAsFactors=F)

# date to date type, Format interval
activ$dateAsDate <- as.Date(activ$date, "%Y-%m-%d")

# set locale and be sure is the right trellis setting
Sys.setlocale("LC_TIME", "English") 

# Make a histogram of the total number of steps taken each day
activ.complete <- activ[complete.cases(activ),]

library(plyr)
byday <- ddply(activ.complete, ~dateAsDate, summarise, stepsPerDay=sum(steps))


hist(byday$stepsPerDay)

mean(byday$stepsPerDay)
median(byday$stepsPerDay)



#------------------------------------------------------------------------------
# What is the average daily activity pattern?
# 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

# 1.
byInterv <- ddply(activ.complete, ~interval, summarise, meanStepsInt=mean(steps))
with(byInterv, plot(interval, meanStepsInt, type="l"))
# 2.
byInterv[which(byInterv$meanStepsInt==max(byInterv$meanStepsInt)),]$interval


#------------------------------------------------------------------------------
# Imputing missing values
# 1. how many?  
sum(!complete.cases(activ))

# strategy: use the mean for that 5 minute interval
# es: 8.35
# 
#byInterv[which(byInterv$interval==835),]$meanStepsInt


activ.fil <- activ
# slow but clear
for(i in which(is.na(activ$steps))) {
    activ.fil[i, "steps"] <- 
        byInterv[which(byInterv$interval==activ.fil[i, "interval"]),]$meanStepsInt
}


sum(!complete.cases(activ.fil))

#------------------------------------------------------------------------------
# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. Do these values 
# differ from the estimates from the first part of the assignment? What is the 
# impact of imputing missing data on the estimates of the total daily number of steps?

byday.fil <- ddply(activ.fil, ~dateAsDate, summarise, stepsPerDay=sum(steps))

hist(byday.fil$stepsPerDay)
mean(byday.fil$stepsPerDay)
median(byday.fil$stepsPerDay)

# steps not altered for complete days. Only new days are being added
with(byday.fil, plot(dateAsDate, stepsPerDay, pch=20, col="red"))
with(byday,     points(dateAsDate, stepsPerDay, pch=20))

#------------------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?
#byday.fil$wday <- "weekday"
#byday.fil[weekdays(byday.fil$dateAsDate, abbreviate=T) %in% c("Sun","Sat"), "wday"] <- "weekend"

activ.fil$wday <- "weekday"
activ.fil[weekdays(activ.fil$dateAsDate, abbreviate=T) %in% c("Sun","Sat"), "wday"] <- "weekend"

byInterv.fil <- ddply(activ.fil, ~ interval + wday, summarise, meanStepsInt=mean(steps))
#with(byInterv.fil, plot(interval, meanStepsInt, type="l"))
str(byInterv.fil)

# final graph
library(lattice)

xyplot(meanStepsInt ~ interval | wday, data=byInterv.fil, layout = c(1,2), type="l",
       ylab="Number of steps",
       xlab="Interval")





