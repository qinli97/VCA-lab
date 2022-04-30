Fix_Stage <- function(df) {
  # input: dataframe with newregion
  # output1: dataframe has an extra column: fix_stage
  # output2: a matrix with mean values of number of fixations inside, outside (and on for middle stage) the annulus for early, middle, late stages
  
  # get all unique subject name in this file
  unique_subject = as.character(unique(df$subject))
  
  # # build an empty dataframe has 3 columns: early, middle, late fixation stages
  # #                                rows: numnber of unique subject names rows (eg: 64 in res3)
  # subject_perct = data.frame(matrix("", ncol = 3, nrow = 64), stringsAsFactors = FALSE)  
  # names(subject_perct) = c("early", "middle","late")
  # row.names(subject_perct) = unique_subject
  # create a new column in data, fix_stage label tham as "middle" at first
  df$fix_stage = 'middle'
  
  # build an empty dataframe has 7 columns: "early1", "early-1","mid1","mid0","mid-1","late1","late-1" 
  #                                rows: numnber of unique subject names rows (eg: 64 in res3)
  #                                entry: each entry is the mean value of the number of fixation     
  subject_percent = data.frame(matrix("", ncol = 7, nrow = 64), stringsAsFactors = FALSE)  
  
  names(subject_percent) = c("early1", "early-1","mid1","mid0","mid-1","late1","late-1")
  row.names(subject_percent) = unique_subject
  
  for (name in unique_subject){
    
    
    num_early = 0  # number of early stage for this subejct
    num_middle = 0  # number of middle stage for this subejct
    num_late = 0  # number of late stage for this subejct
    
    early_pos = 0 # fixations outside the annulus in early phase
    early_neg = 0 # fixations inside the annulus in early phase
    mid_pos = 0 # fixations outside the annulus in middle phase
    mid_zero = 0 # fixations on the annulus in middle phase
    mid_neg = 0 # fixations inside the annulus in early phase
    late_pos = 0 # fixations outside the annulus in late phase
    late_neg = 0 # fixations inside the annulus in late phase
    
    temp_subject_index = which(df$subject %in% name) # get all index of current subject
    subject_len = length(temp_subject_index)  # length of fixations across trials for this subject
    start_subject_index = min(temp_subject_index) # get start index
    end_subject_index = max(temp_subject_index) # get end_index
    
    # get unique trial number for current subject
    temp_unique_trial = unique(df[start_subject_index:end_subject_index,]$trial)
    
    
    trial_len = length(temp_unique_trial) # number of unique trials
    len_subject = length(temp_subject_index) # number of fixations across trials
    for (temp_trial in temp_unique_trial){
      #print(which(df[start_subject_index:end_subject_index,]$trial %in% temp_trial) + start_subject_index - 1)
      
      # get all index of this current trial
      temp_trial_index = which(df[start_subject_index:end_subject_index,]$trial %in% temp_trial) +start_subject_index - 1
      
      start_trial_index = min(temp_trial_index) # start index of this trial
      end_trial_index = max(temp_trial_index) # end index of this trial
      #cat(start_trial_index,end_trial_index,'\n')
      
      for (i in c(start_trial_index:end_trial_index)){ # indexing from the start of this trial
        if (df[i,]$newregion != 0){ 
          df[i,]$fix_stage = 'early' # if newregion is not 0 (on the annulus), overwrite it as "early"
          # num_early = num_early + 1 # culmulate the number of early fixation
          if (df[i,]$newregion == 1){
            early_pos = early_pos + 1 # add 1 to current number of fixation outside the annulus in early phase
          }
          else{
            early_neg = early_neg + 1 # add 1 to current number of fixation inside the annulus in early phase
          }
        }
        
        else{ # stop indexing if newregion at this index is 0

          break
        }
      }
      #print(end_index)
      if (i + 1 <= end_trial_index){ # check if the index stays in the same trial

        for (j in c(end_trial_index:(i + 1))){  # go backwards in this trial to label anything not 0 as "late"
          if (df[j,]$newregion != 0){
            df[j,]$fix_stage = 'late' # if newregion is not 0 (on the annulus), overwrite it as "late"
            # num_late = num_late + 1 # culmulate the number of late fixation
            if (df[j,]$newregion == 1){
              late_pos = late_pos + 1 # add 1 to current number of fixation outside the annulus in late phase
            }
            else{
              late_neg = late_neg + 1 # add 1 to current number of fixation inside the annulus in late phase
            }
          }
          else{ # stop if 0 appears
            #print(j)
            break
          }
        }
      }
      for (i in c(start_trial_index:end_trial_index)){
        #print(i)
        if (df[i,]$fix_stage == "middle"){
          
          if (df[i,]$newregion == 1){
            mid_pos = mid_pos + 1 # add 1 to current number of fixation outside the annulus in middle phase
          }
          else if (df[i,]$newregion == 0){
            mid_zero = mid_zero + 1 # add 1 to current number of fixation on the annulus in middle phase
          }
          else if (df[i,]$newregion == -1){
            mid_neg = mid_neg + 1 # add 1 to current number of fixation inside the annulus in middle phase
          }
        }
      }

    }
    # subject_perct[name,"early"] = num_early # number of early fixations in this subject
    # subject_perct[name, "late"] = num_late # number of late fixations in this subject
    # subject_perct[name, "middle"] = subject_len - num_early - num_late #  number of middle fixations in this subject
    #                                                                    #  substract number of early and late 
    #                                                                    #  from the number of indexes in this subject
    subject_percent[name, "early1"] = (early_pos)/trial_len # mean value of early fixations outside the annulus in this subject across trials
    subject_percent[name, "early-1"] = (early_neg)/trial_len # mean value of early fixations inside the annulus in this subject across trials
    subject_percent[name, "mid1"] = (mid_pos)/trial_len # mean value of middle fixations outside the annulus in this subject across trials
    subject_percent[name, "mid0"] = (mid_zero)/trial_len # mean value of middle fixations on the annulus in this subject across trials
    subject_percent[name, "mid-1"] = (mid_neg)/trial_len # mean value of middle fixations inside the annulus in this subject across trials
    subject_percent[name, "late1"] = (late_pos)/trial_len # mean value of late fixations outside the annulus in this subject across trials
    subject_percent[name, "late-1"] = (late_neg)/trial_len # mean value of late fixations inside the annulus in this subject across trials
  }
  #res3_part <- write.csv(df, file = "res3_part.csv")
  #subject_pctv1 <- write.csv(subject_perct, file = "subject_pctv1.csv")
  return(df)

}
