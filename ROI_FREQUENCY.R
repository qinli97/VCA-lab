sub1 = subset(res3, fix_stage == "middle") # eliminate trials have no "middle" fixation stage
sub1 = subset(sub1, acc == 1) # only look at trials got correct responses
sub1_p = subset(sub1, pa == 1)  # target present
sub1_a = subset(sub1, pa == 0)  # target absent
ROIindex_frequency <- function(sub1){
  ## use table() to know the frequency of each number in a list
  unique_subject = as.character(unique(sub1$subject))
  ROI_frequency = data.frame(matrix(0, ncol = 10, nrow = 64), stringsAsFactors = FALSE)  
  
  names(ROI_frequency) = c("ROI_1","ROI_2", "ROI_3", "ROI_4","ROI_5", "ROI_6", "ROI_7","ROI_8","ROI_9","ROI_10")
  row.names(ROI_frequency) = unique_subject
  unique_trial = unique(sub1$trial)
  
  for (name in unique_subject){
    temp_ROI = data.frame(matrix("", ncol = 1, nrow = 256), stringsAsFactors = FALSE)
    names(temp_ROI) = "ROI_trial"
    #row.names(temp_ROI) = unique_trial
    temp_subject_index = which(sub1$subject %in% name) # get all index of current subject
    start_subject_index = min(temp_subject_index) # get start index
    end_subject_index = max(temp_subject_index) # get end_index
    
    # get unique trial number for current subject
    temp_unique_trial = unique(sub1[start_subject_index:end_subject_index,]$trial)
    for (temp_trial in temp_unique_trial){
      #print(which(sub1[start_subject_index:end_subject_index,]$trial %in% temp_trial) + start_subject_index - 1)
      
      # get all index of this current trial
      temp_trial_index = which(sub1[start_subject_index:end_subject_index,]$trial %in% temp_trial) +start_subject_index - 1
      
      start_trial_index = min(temp_trial_index) # start index of this trial
      end_trial_index = max(temp_trial_index) # end index of this trial
      #cat(start_trial_index,end_trial_index,'\n')
  
      temp_ROI[temp_trial,] = sub1[start_trial_index,]$ROIindex # the first fixation in this trial

    }
    tab_ROI = as.data.frame(table(temp_ROI),stringsAsFactors = FALSE) 
    '%ni%' = Negate('%in%') # not in
    tab_ROI = tab_ROI[-c(tab_ROI$temp_ROI == ""),] # delete the trials has no "middle" fixation
    miss_ROI = which(roi_ind %ni% (tab_ROI$temp_ROI)) # in case the subject didn't have their first fixation on some ROI indexes 
    if (length(miss_ROI) != 0){ # if there is a ROI index hasn't been looked at (for first fixation on each trial)
      for (m in miss_ROI){ # in case there are more than one ignored ROI indexes
      tab_ROI[nrow(tab_ROI)+1,] = c(m,0)
      }
    }
    ROI_frequency[name,"ROI_1"] = tab_ROI[(tab_ROI$temp_ROI == "1"),]$Freq # paste the frequency of ROI_1 to its subject
    ROI_frequency[name,"ROI_2"] = tab_ROI[(tab_ROI$temp_ROI == "2"),]$Freq 
    ROI_frequency[name,"ROI_3"] = tab_ROI[(tab_ROI$temp_ROI == "3"),]$Freq
    ROI_frequency[name,"ROI_4"] = tab_ROI[(tab_ROI$temp_ROI == "4"),]$Freq
    ROI_frequency[name,"ROI_5"] = tab_ROI[(tab_ROI$temp_ROI == "5"),]$Freq
    ROI_frequency[name,"ROI_6"] = tab_ROI[(tab_ROI$temp_ROI == "6"),]$Freq
    ROI_frequency[name,"ROI_7"] = tab_ROI[(tab_ROI$temp_ROI == "7"),]$Freq
    ROI_frequency[name,"ROI_8"] = tab_ROI[(tab_ROI$temp_ROI == "8"),]$Freq
    ROI_frequency[name,"ROI_9"] = tab_ROI[(tab_ROI$temp_ROI == "9"),]$Freq
    ROI_frequency[name,"ROI_10"] = tab_ROI[(tab_ROI$temp_ROI == "10"),]$Freq
  }
  ROI_Frequency <- write.csv(ROI_frequency, file = "res3_ROI_Frequency.csv")
  return(ROI_frequency)
}

# tab_ROI = as.data.frame(tab_ROI)
# tab_ROI = tab_ROI[-c(tab_ROI$temp_ROI == ""),]
# which(roi_ind %ni% (tab_ROI$temp_ROI))


