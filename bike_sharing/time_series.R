library(ts)

month = bike.train[bike.train$date >= '2012-04-01' & bike.train$date < '2012-06-01', ]
hour12 = month[month$hour == 12,]
hour9 = month[month$hour == 9,]
hour18 = month[month$hour == 18,]
plot(hour12$date, hour12$registered)
plot(hour12$date, hour12$casual)
plot(hour9$date, hour9$registered)
plot(hour18$date, hour18$count)

