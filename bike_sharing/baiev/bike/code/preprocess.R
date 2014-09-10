preprocess <- function(path.to.data){
  dataset <- read.csv(path.to.data, header = TRUE, sep = ",", dec = ".")
  print('data set reading - done');
  
  ## ----------------------------
  ## Working day and holiday convolution
  ## ----------------------------
  # show that holiday and workingday can be convolved
  #   nrow(dataset) == length(dataset$holiday[(dataset$holiday == 1 | dataset$workingday == 0)])  +
  #     length(dataset$workingday[dataset$workingday == 1])
  
#   wd_idx <- grep('workingday', names(dataset))
#   hol_idx <- grep('holiday', names(dataset))
#   
#   dataset$workingday <- apply(dataset[,c(wd_idx,hol_idx)], MARGIN=1, FUN=function(x){
#         if (x[1] == 1) return (1) else return (0)
#       })
#   dataset <- dataset[,-hol_idx]
#   
#   print('Working day and holiday convolution - done');
#   
  ## ----------------------------
  ## datetime convertion
  ## ----------------------------
  ds <- as.POSIXct(strptime(dataset$datetime, format="%Y-%m-%d %H:%M:%S"))
  dataset$sec <- unclass(ds)
  ds <- as.POSIXlt(strptime(dataset$datetime, format="%Y-%m-%d %H:%M:%S"))
#   dataset$month <- ds$mon
  dataset$day <- ds$mday
  dataset$hour <- ds$hour
  dataset$wday <- ds$wday

  print('datetime convertion - done');

  ## ----------------------------
  ## set weather "4" to "3"
  ## ----------------------------
  
  dataset$weather[dataset$weather == 4] <- 3
    
  print('set weather 4 to 3 - done');

  ## ----------------------------
  ## get humidity * windspeed
  ## ----------------------------

  dataset$hw <- dataset$humidity + dataset$windspeed
  print('get humidity * windspeed - done');

  return(dataset)
  
}

