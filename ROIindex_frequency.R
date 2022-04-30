ROIindex_frequency <- function(df){
  ## use table() to know the frequency of each number in a list
  unique_subject = as.character(unique(df$subject))
  ROI_frequency = data.frame(matrix("", ncol = 10, nrow = 64), stringsAsFactors = FALSE)  
  
  names(subject_percent) = c("ROI_1","ROI_2", "ROI_3", "ROI_4","ROI_5", "ROI_6", "ROI_7","ROI_8","ROI_9","ROI_10")
  row.names(subject_percent) = unique_subject
  unique_trial = as.character(unique(df$trial))
  
  
  for (name in unique_subject){
    temp_ROI = data.frame(matrix("", ncol = 1, nrow = 256), stringsAsFactors = FALSE)
    names(temp_ROI) = "ROI_trial"
    row.names(temp_ROI) = unique_trial
    temp_subject_index = which(df$subject %in% name) # get all index of current subject
    subject_len = length(temp_subject_index)  # length of fixations across trials for this subject
    start_subject_index = min(temp_subject_index) # get start index
    end_subject_index = max(temp_subject_index) # get end_index
    
    # get unique trial number for current subject
    temp_unique_trial = unique(df[start_subject_index:end_subject_index,]$trial)
    for (temp_trial in temp_unique_trial){
      #print(which(df[start_subject_index:end_subject_index,]$trial %in% temp_trial) + start_subject_index - 1)
      
      # get all index of this current trial
      temp_trial_index = which(df[start_subject_index:end_subject_index,]$trial %in% temp_trial) +start_subject_index - 1
      
      start_trial_index = min(temp_trial_index) # start index of this trial
      end_trial_index = max(temp_trial_index) # end index of this trial
      #cat(start_trial_index,end_trial_index,'\n')
      
      for (i in c(start_trial_index:end_trial_index)){ # indexing from the start of this trial
        while (df[i,]$newregion == 0){
          temp_ROI[temp_trial] = df[i,]$ROIindex
          
        }
        

    }
    }
  }
}

