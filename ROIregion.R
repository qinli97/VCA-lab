ROIregion <- function(data) {
  
  data[, 'newregion'] <- 0
  

  for (i in 1:nrow(data)){
    if (data$r[i] <150){
      data$newregion[i] = -1
    }else if (data$r[i] >250) {
      data$newregion[i] <- 1}

  }
  return(data)
  
}