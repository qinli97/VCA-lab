# for each participant, select fixations in one trial and enumerate them

newfixIndex <- function(data){
  
  data <- subset(data, data$CURRENT_FIX_START > 0) # save only fixation that occurred after the onset of search array
  
  
  # adjust fixIndex after rejecting negative fixation (occurred before search)
  # use this after removing negative fixations 
  indexValue = 2
  data$newfixIndex[1]<- 1 # assign the first fixation index. new fixation index starts 
  for (j in 2:nrow(data)){ # since I've assinged the first index number the loop starts from 2. 
    if (data$trial[j] != data$trial[j-1] ){ # whenever new trial starts, 
      indexValue = 1 # assign the index number to 1. 
    }
    data$newfixIndex[j]<- indexValue # if the loop is still within a trial, assign the next index number
    indexValue <- indexValue +1 # add 1 to the current index number 
  }
  return(data)
}
