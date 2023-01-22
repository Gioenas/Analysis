#import useful libraries
library(dplyr)

label_names <- function(data_col){
  #function for inserting proper labels in the column activity instead of integer
  #for better describe the activity
  
  f <- c("walking", "walking_upstairs", "walking_downstairs",
         "sitting", "standing", "laying")
  num <- length(data_col)
  
  for(i in 1:num){
    
    if(data_col[i] == 1) {data_col[i] =f[1]}
    else if(data_col[i] == 2) {data_col[i] = f[2]}
    else if(data_col[i] == 3) {data_col[i] = f[3]}
    else if(data_col[i] == 4) {data_col[i] = f[4]}
    else if(data_col[i] == 5) {data_col[i] = f[5]}
    else if(data_col[i] == 6) {data_col[i] = f[6]}
    else {message("nuber inserted out of bounds")}
    
  }
  data_col
}

sort_activity <- function(db){ #function for selecting a database for each subject
  #and each activity
  
  new_df <-data.frame()
  for(n in 1:30){
    for(i in 1:6){
      
      new_row <- db %>% filter( subject == n & activity == i) %>% select(-str_activity) %>%
        lapply(mean)
      new_df <- rbind(new_df, new_row)
    }
  }
  new_df
}
PATH <- getwd() #the working directory


#read X_train and X_test as data.tables
X_test <- read.table(paste0(PATH, "/UCI/test/X_test.txt"))
X_train <- read.table(paste0(PATH, "/UCI/train/X_train.txt"))

#read the subject column
subj_test <- read.table(paste0(PATH, "/UCI/test/subject_test.txt"))
subj_train <- read.table(paste0(PATH, "/UCI/train/subject_train.txt"))
#add the subject column
X_test$subject <- subj_test$V1
X_train$subject <- subj_train$V1

#read the activities column
activit_test <- read.table(paste0(PATH, "/UCI/test/y_test.txt"))
activit_train <- read.table(paste0(PATH, "/UCI/train/y_train.txt"))
#add the activities columns
X_test$activity <- activit_test$V1
X_train$activity <- activit_train$V1

# read the labels as data.table from features.txt
labels <- read.table(paste0(PATH, "/UCI/features.txt"))
#extract the label names
names <- labels$V2
#append the name of the subject column
names <- append(names, c("subject", "activity"))

#format and clean the features labels
names <- gsub("\\()", "_", names)
names <- gsub("-", "_", names)
names <- gsub("^fBody", "", names)
names <- gsub("^tBody", "Total", names)
names <- gsub("^t", "Total", names)
names <- gsub( "_$", "",names)
#merge 2 dataset
X_merged <- merge(X_train, X_test, all = TRUE)

#change the label names with descriptive ones on the merged dataset
colnames(X_merged) <- names

#select all labels with mean and standard deviation features indexes
sel_std <- grep("std", names)
sel_mean <- grep("mean", names)

#select the names of the labels for the mean and standard deviation
name_std <- names[sel_std]
name_mean <- names[sel_mean]
#put them together selecting the names with mean, standard deviation, subject
#and activity 
labels <- append(name_mean, name_std)
labels <- append(labels, c("subject", "activity"))

#subset the dataset with only the values for mean, std, subject and activity
#with proper descriptive labels
X_selected <- select(X_merged, labels)
X_selected$str_activity <- label_names(X_selected$activity)

#write the database and save to file
write.csv(X_selected, paste0(PATH, "/X_selected.csv"))

#make a copy of the database to select a new one
X_activity <- X_selected
# construct a final database with the means for each subject and each activity
X_sorted <- sort_activity(X_activity)
#change he value of activity column with descriptive ones
X_sorted$activity <- label_names(X_sorted$activity)

#write the database and save to file
write.csv(X_sorted, paste0(PATH, "/X_sorted.csv"))







