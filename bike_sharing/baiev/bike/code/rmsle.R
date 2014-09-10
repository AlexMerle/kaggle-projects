rmsle <- function(val.true, val.pred){
  return (sqrt(mean((log(val.pred+1) - log(val.true+1))^2)))
}

