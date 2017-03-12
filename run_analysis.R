# 
# Coursera Peer-graded Assignment: Getting and Cleaning Data Course Project
# 
# The purpose of this project is to demonstrate ability to collect, work with,
# and clean a data set.
# 
# Author: Marçal Nebot i Manyà, https://github.com/mnebot,
# http://linkedin.com/in/mnebot
# 
# Row data Information:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Script purpose:
# 
# 1.Merges the training and the test sets to create one data set. 
# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.Uses descriptive activity names to name the activities in the data set 
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject.
# 
# And finnaly create a file with the tidy data set for assigment submission
# 



# load necessary librarys
library("dplyr")

# download row data
fileURl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURl,"UCIML.zip")

# unzip row data 
unzip("UCIML.zip")

# read files
features            <- read.csv("./UCI HAR Dataset/features.txt",header = FALSE,sep = " ")
train               <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
test                <- read.table("./UCI HAR Dataset/test/X_test.txt",sep = "")
train_activities    <- read.csv("./UCI HAR Dataset/train/y_train.txt",header = FALSE)
test_activities     <- read.csv("./UCI HAR Dataset/test/y_test.txt",header = FALSE)
train_subjects      <- read.csv("./UCI HAR Dataset/train/subject_train.txt",header = FALSE)
test_subjects       <- read.csv("./UCI HAR Dataset/test/subject_test.txt",header = FALSE)
activities          <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = "")

# set attribute names on columns to train and test datasets
atributeNames <-  features$V2
names(train) <- atributeNames
names(test) <- atributeNames

# filter columns by mean and std to train and test datasets
# first of all is necessary to delete not unique data
train <- train[ !duplicated(names(train)) ]
test <- test[ !duplicated(names(test)) ]

toMatch <- c("mean", "std")
train <- select(train, matches(paste(toMatch,collapse="|")))
test <- select(test, matches(paste(toMatch,collapse="|")))

# add activity and subject columns to test and train datasets 
# before adding activity and subject columns set column names
names(train_activities) <- c("Activity")
names(test_activities) <- c("Activity")
names(train_subjects) <- c("Subject")
names(test_subjects) <- c("Subject")

train <- cbind(train,train_activities,train_subjects)
test <- cbind(test,test_activities,test_subjects)

# merge test and train datasets
merged <- rbind(train,test)

# Appropriately labels the data set with descriptive variable names
names(activities) <- c("id", "activityname")
merged <- merge(merged, activities, by.x = "Activity", by.y = "id")
# remove Activity column for a tidy dataset
merged <- merged[, ! names(merged) %in% "Activity", drop = F]
merged <- merged[, ! names(merged) %in% "id", drop = F]

# create dataset with  the average of each variable for each activity 
# and each subject
subjectActivity <- group_by(merged,Subject, activityname)
tidyDF <- summarise_each(subjectActivity,funs(mean))

# create a txt file from the dataset created 
write.table(tidyDF,row.names = F,file = "CourseraAssigment_MNM_tidyDS.txt") 