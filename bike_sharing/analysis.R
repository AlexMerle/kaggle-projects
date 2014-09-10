
str(bike.train)

plot(bike.train$date, bike.train$count)
plot(bike.train[bike.train$date >= '2012-05-01' & bike.train$date < '2012-06-01', ]$date, bike.train[bike.train$date >= '2012-05-01' & bike.train$date < '2012-06-01', ]$count)

dim(bike.train[bike.train$date >= '2012-05-01' & bike.train$date < '2012-06-01', ])

bike.train[bike.train$weather == 4, ]

plot(bike.train$hours, bike.train$count)

plot(bike.train[bike.train$workingday == 1,]$hours, bike.train[bike.train$workingday == 1,]$count)
plot(bike.train[bike.train$workingday == 1,]$hours, bike.train[bike.train$workingday == 1,]$count)
plot(bike.train[bike.train$workingday == 0,]$hours, bike.train[bike.train$workingday == 0,]$count)

plot(bike.train[bike.train$workingday == 1,]$hours, bike.train[bike.train$workingday == 1,]$registered)
plot(bike.train[bike.train$workingday == 0,]$hours, bike.train[bike.train$workingday == 0,]$registered)

plot(bike.train[bike.train$workingday == 1,]$hours, bike.train[bike.train$workingday == 1,]$casual)
plot(bike.train[bike.train$workingday == 0,]$hours, bike.train[bike.train$workingday == 0,]$casual)


boxplot(bike.train$count~bike.train$date)
boxplot(bike.train$count~bike.train$season)
boxplot(bike.train$count~bike.train$holiday)
boxplot(bike.train$count~bike.train$workingday)
boxplot(bike.train$count~bike.train$weather)

plot(bike.train$atemp, bike.train$windspeed)

plot(bike.train$temp, bike.train$count)
plot(bike.train$atemp, bike.train$count)
plot(bike.train$humidity, bike.train$count)
plot(bike.train$windspeed, bike.train$count)

plot(bike.train$atemp, bike.train$humidity)
plot(bike.train$temp, bike.train$atemp) # issue found
plot(bike.test$temp, bike.test$atemp)

boxplot(bike.train$casual~bike.train$season)
boxplot(bike.train$registered~bike.train$season)

boxplot(bike.train$count~bike.train$weather)
boxplot(bike.train$count~bike.train$month)

table(bike.train$weather)
table(bike.train$holiday)

# group by time
# time series
# avg by day time series

plot(bike.train$atemp,bike.train$humidity)
boxplot(bike.train$humidity)


qplot(hours, count, data=bike.train) + geom_bar(stat="identity")
#qplot(hours, count, data=bike.train, geom="boxplot")

boxplot(bike.train$count ~ bike.train$year)
