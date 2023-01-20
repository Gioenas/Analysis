#import useful libraries
library(dplyr)
#read X_train and X_test as data.tables
X_test <- read.table("~/data/UCI/test/X_test.txt")
X_train <- read.table("~/data/UCI/train/X_train.txt")

# read the labels as data.table from features.txt
labels <- read.table("~/data/UCI/features.txt")
#extract the label names
names <- labels$V2
#merge 2 datasets
X_merged <- merge(X_train, X_test, all = TRUE)
#select the label names
colnames(X_merged) <- names
#select all labels with mean and standard deviation features indexes
sel_std <- grep("std", names)
sel_mean <- grep("mean", names)
#select the actual names of the labels
name_std <- names[sel_std]
name_mean <- names[sel_mean]
#the entire list of names of the features with mean and standard deviation
labels <- append(name_mean, name_std)
#subset the dataset with only the values for mean and std with proper descriptive
#labels
X_selected <- select(X_merged, labels)
#write the database and save to file

write.csv(X_selected, "~/R_Coursera/Analysis/X_selected.csv")

