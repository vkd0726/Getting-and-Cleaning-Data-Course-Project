#Week 3 - Getting and Cleaning Data Course Project

#File details
fileName <- "week3AssignmentDataset.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


#Getting data
if(!file.exists(fileName))
{
  download.file(fileUrl, fileName, method = "curl")
  unzip(fileName)
}


#Set path
dataPath <- "UCI HAR Dataset"

##Load libraries
library(plyr)
library(data.table)

##### TASK 1 ######
#1. Merges the training and the test sets to create one data set

#######Read Data########

# read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Concatinate data
typeOfActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# Assigning Column Names
colnames(typeOfActivity) <- c("subject", features[, 2], "activity")

##### TASK 2 ######
#2.Extracts only the measurements on the mean and standard deviation for each measurement

# Grep for subject, activity and mean/std columns
columnsToKeep <- grepl("subject|activity|mean|std", colnames(typeOfActivity))

# Subsetting
typeOfActivity <- typeOfActivity[, columnsToKeep]


##### TASK 3 ######
#3.Uses descriptive activity names to name the activities in the data set

typeOfActivity$activity <- factor(typeOfActivity$activity, levels = activities[,1], labels = activities[,2])
typeOfActivity$subject <- as.factor(typeOfActivity$subject)


##### TASK 4 ######
#4. Appropriately label the data set with descriptive variable names

# get column names
updateColName <- colnames(typeOfActivity)

# remove special characters
updateColName <- gsub("[\\(\\)-]", "", updateColName)

# Update the abbrevations
updateColName <- gsub("^f", "frequencyDomain", updateColName)
updateColName <- gsub("^t", "timeDomain", updateColName)
updateColName <- gsub("Acc", "Accelerometer", updateColName)
updateColName <- gsub("Gyro", "Gyroscope", updateColName)
updateColName <- gsub("Mag", "Magnitude", updateColName)
updateColName <- gsub("Freq", "Frequency", updateColName)
updateColName <- gsub("mean", "Mean", updateColName)
updateColName <- gsub("std", "StandardDeviation", updateColName)

# correction
updateColName <- gsub("BodyBody", "Body", updateColName)

# use new labels as column names
colnames(typeOfActivity) <- updateColName
View(typeOfActivity)

##### TASK 5 ######
#5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject

library(reshape2)
typeOfActivity.melted <- melt(typeOfActivity, id = c("subject", "activity"))
typeOfActivity.mean <- dcast(typeOfActivity.melted, subject + activity ~ variable, mean)

write.table(typeOfActivity.mean, "tidy.txt", row.names = FALSE, quote = FALSE)