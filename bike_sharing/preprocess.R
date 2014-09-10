library("lubridate")

load_file <- function(filename) {
  setwd("C:\\MyDocs\\kaggle\\bike_sharing")
  return (read.csv(filename))
}

preprocess <- function(data) {
  firstdate = as.Date("2011-01-01")
  
  data$datetime.orig = data$datetime
  data$datetime = as.POSIXlt(data$datetime.orig)
  data$date = as.Date(data$datetime.orig)
  data$season = as.factor(data$season)
  data$holiday = as.factor(data$holiday)
  data$workingday = as.factor(data$workingday)
  data$weather = as.factor(data$weather)
  data$hour = as.integer(format(as.POSIXct(data$datetime.orig), format="%H"))
  data$year = as.integer(format(as.POSIXct(data$datetime.orig), format="%Y"))
  data$month = as.integer(format(as.POSIXct(data$datetime.orig), format="%m"))
  data$dayofweek = as.integer(wday(as.Date(data$datetime.orig)))
  data$monthyear = (data$year-2011)*12+data$month
  data$daynumber = as.integer(data$date-firstdate)
  
  data[data$weather == 4,]$weather = 3
  data$weather = as.factor(as.integer(data$weather))
  
  return (data)
}

subsetByDayOfMonth <- function(data, numOfDay) {
  return (data[as.integer(format(as.POSIXct(data$datetime.orig), format="%d"))==numOfDay,])
}

fix.train <- function(data) {
  # fix atemp = 12.12 (2012-08-17)
  data[data$temp > 25 & data$atemp == 12.12,]
  
  data[data$temp == 27.88 & data$atemp == 12.12,]$atemp = median(data[data$temp == 27.88,]$atemp)
  data[data$temp == 27.06 & data$atemp == 12.12,]$atemp = median(data[data$temp == 27.06,]$atemp)
  data[data$temp == 26.24 & data$atemp == 12.12,]$atemp = median(data[data$temp == 26.24,]$atemp)
  data[data$temp == 25.42 & data$atemp == 12.12,]$atemp = median(data[data$temp == 25.42,]$atemp)
  data[data$temp == 28.70 & data$atemp == 12.12,]$atemp = median(data[data$temp == 28.70,]$atemp)
  data[data$temp == 30.34 & data$atemp == 12.12,]$atemp = median(data[data$temp == 30.34,]$atemp)
  data[data$temp == 33.62 & data$atemp == 12.12,]$atemp = median(data[data$temp == 33.62,]$atemp)
  data[data$temp == 35.26 & data$atemp == 12.12,]$atemp = median(data[data$temp == 35.26,]$atemp)
  data[data$temp == 31.16 & data$atemp == 12.12,]$atemp = median(data[data$temp == 31.16,]$atemp)
  data[data$temp == 34.44 & data$atemp == 12.12,]$atemp = median(data[data$temp == 34.44,]$atemp)
  data[data$temp == 29.52 & data$atemp == 12.12,]$atemp = median(data[data$temp == 29.52,]$atemp)
  
  # fix humidity = 0 (2011-03-10)
  data[data$humidity == 0,]
  
  data[data$atemp == 15.91 & data$humidity == 0,]$humidity = median(data[data$atemp == 15.91 & data$weather == 3,]$humidity)
  data[data$atemp == 17.425 & data$humidity == 0,]$humidity = median(data[data$atemp == 17.425 & data$weather == 3,]$humidity)
  data[data$atemp == 16.665 & data$humidity == 0,]$humidity = median(data[data$atemp == 16.665 & data$weather == 3,]$humidity)
  data[data$atemp == 19.695 & data$humidity == 0,]$humidity = median(data[data$atemp == 19.695 & data$weather == 3,]$humidity)
  data[data$atemp == 20.455 & data$humidity == 0,]$humidity = median(data[data$atemp == 20.455 & data$weather == 3,]$humidity)
  data[data$atemp == 21.97 & data$humidity == 0,]$humidity = median(data[data$atemp == 21.97 & data$weather == 3,]$humidity)
  data[data$atemp == 21.21 & data$humidity == 0,]$humidity = median(data[data$atemp == 21.21 & data$weather == 3,]$humidity)
  
  return(data)
}

fix.test <- function(data) {
  data$hour[630] <- data$hour[629] + 1
  data$hour[3872] <- data$hour[3871] + 1
  
  return (data)
}

get.cv_train <- function(train) {
  data = rbind(subsetByDayOfMonth(train, 1),subsetByDayOfMonth(train, 2),subsetByDayOfMonth(train, 3),
                subsetByDayOfMonth(train, 4),subsetByDayOfMonth(train, 5),subsetByDayOfMonth(train, 6),
                subsetByDayOfMonth(train, 7),subsetByDayOfMonth(train, 8),subsetByDayOfMonth(train, 9),
                subsetByDayOfMonth(train, 10),subsetByDayOfMonth(train, 11),subsetByDayOfMonth(train, 12),
                subsetByDayOfMonth(train, 13),subsetByDayOfMonth(train, 14),subsetByDayOfMonth(train, 15),
                subsetByDayOfMonth(train, 16))
  return (data)
}

get.cv_valid <- function(train) {
  data = rbind(subsetByDayOfMonth(train, 19),subsetByDayOfMonth(train, 18),subsetByDayOfMonth(train, 17))
  return (data)
}