# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data  

```r
Sys.setlocale(category = "LC_ALL", locale = "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
dataTable <- read.csv("activity.csv", colClasses = "character")
dataTable$steps <- as.numeric(dataTable$steps)
dataTable$date <- as.Date(dataTable$date, "%Y-%m-%d")
dataTable$interval <- as.numeric(dataTable$interval)
dateList <- sort(dataTable[!duplicated(dataTable[,"date"]),"date"])
intervalList <- sort(dataTable[!duplicated(dataTable[,"interval"]),"interval"])
```

## What is mean total number of steps taken per day?  

```r
dtStats <- data.frame(date = character(0), steps = numeric(0))
dtStats$date <- as.Date(dtStats$date, "%Y-%m-%d")

for(currentDate in dateList)
{
  dailySteps <- dataTable[currentDate == dataTable$date, "steps"]
  dailySum <- sum(dailySteps, na.rm = TRUE)
  
  tempDT <- data.frame(date = currentDate, steps = dailySum)
  tempDT$date <- as.Date(tempDT$date, origin="1970-01-01")
  
  dtStats <- rbind(dtStats, tempDT)
}

meanSteps <- mean(dtStats$steps)
medianSteps <- median(dtStats$steps)
```
The mean is: 9354.2295082  
The median is: 1.0395\times 10^{4}  

## What is the average daily activity pattern?  

```r
count <- as.numeric(0)
daStats <- data.frame(interval = numeric(0), steps = numeric(0))

for(currentInterval in intervalList)
{
  count <- count + 1
  stepStore <- dataTable[currentInterval == dataTable$interval, "steps"]
  numInstanceInterval <- length(stepStore)
  avgSteps <- sum(stepStore, na.rm = TRUE) / numInstanceInterval
  
  tempDT <- data.frame(interval = currentInterval, steps = avgSteps)
  
  daStats <- rbind(daStats, tempDT)
}

maxInterval = daStats[max(daStats$steps), "interval"]
```
The 5-minute interval that contains the maximum number of steps is: 1450  


## Imputing missing values  

```r
revDataTable <- dataTable

numNA <- length(revDataTable[is.na(revDataTable$steps), "steps"])

for(currentDate in dateList)
{
  dailySteps <- revDataTable[currentDate == revDataTable$date, "steps"]
  numDailySteps <- length(dailySteps[!is.na(dailySteps)])
  if(numDailySteps > 0)
  {
    revDataTable[(currentDate == revDataTable$date) & is.na(revDataTable$steps), "steps"] <- dtStats[(currentDate == dtStats$date), "steps"] / numDailySteps
  }
}

for(currentInterval in intervalList)
{
  intervalSteps <- revDataTable[currentInterval == revDataTable$interval, "steps"]
  numIntervalSteps <- length(intervalSteps[!is.na(intervalSteps)])

  revDataTable[(currentInterval == revDataTable$interval) & is.na(revDataTable$steps), "steps"] <- daStats[(currentInterval == daStats$interval), "steps"]
}

revStats <- data.frame(date = character(0), steps = numeric(0))
revStats$date <- as.Date(revStats$date, "%Y-%m-%d")

for(currentDate in dateList)
{
  dailySteps <- revDataTable[currentDate == revDataTable$date, "steps"]
  dailySum <- sum(dailySteps, na.rm = TRUE)
  
  tempDT <- data.frame(date = currentDate, steps = dailySum)
  tempDT$date <- as.Date(tempDT$date, origin="1970-01-01")
  
  revStats <- rbind(revStats, tempDT)
}

revMeanSteps <- mean(revStats$steps)
revMedianSteps <- median(revStats$steps)
```
Daily averages were used to fill the NA values whenever daily averages are avaliable. If daily averages are not avaliable, the averages of the 5 minute time slot across all dates for the time slot having the NA value is used.

The revised mean is: 1.0581014\times 10^{4}  
The revised median is: 1.0395\times 10^{4}  

## Are there differences in activity patterns between weekdays and weekends?

