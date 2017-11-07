# Load the packages:
library(reshape2)

# Set the local directory for the data download destination + Script
setwd("C:/MLCourse/machine-learning-ex3")

# Download the data and unzip it in the local directory 
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "getdata_dataset.zip"
download.file(URL, file)
unzip(file) 

#####
# 1. Merges the training and the test sets to create one data set.
#####

# Load the datasets and merge them columnwise - first "train" and then "test"

train_X <- read.table("train/X_train.txt")
train_Y <- read.table("train/Y_train.txt")
train_subject <- read.table("train/subject_train.txt")
train <- cbind(train_subject,train_X, train_Y)

test_X <- read.table("test/X_test.txt")
test_Y <- read.table("test/Y_test.txt")
test_subject <- read.table("test/subject_test.txt")
test <- cbind(test_subject,test_X, test_Y)

# merge datasets' rows - all rows from the train and test datasets are added in one dataset "mergedTrainTest"

mergedTrainTest <- rbind(train, test)

# Extract the feature names from file

features <- read.table("features.txt", as.is = TRUE)


# the the column names

colnames(mergedTrainTest) <- c("subject", features[, 2], "activity")



#####
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#####

extractedColumns <- grepl("subject|activity|mean|std", colnames(mergedTrainTest)) # Keep only columns containing "subject","activity","mean" or "std"
mergedTrainTest <- mergedTrainTest[, extractedColumns]


#####
# 3.  Uses descriptive activity names to name the activities in the data set
#####

# Extract the activity labels from file.

activities <- read.table("activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")

# turn activities & subjects into factors

mergedTrainTest$activity <- factor(mergedTrainTest$activity, levels = activities[,1], labels = activities[,2])
mergedTrainTest$subject <- as.factor(mergedTrainTest$subject)


#####
# 4. Appropriately labels the data set with descriptive variable names.
#####

# get column names

mergedTrainTestCols <- colnames(mergedTrainTest)

# clean column names (remove non descriptive characters)

mergedTrainTestCols <- gsub("[\\(\\)-]", "", mergedTrainTestCols)

# set new names

mergedTrainTestCols <- gsub("^f", "FrequencyDomain", mergedTrainTestCols)
mergedTrainTestCols <- gsub("^t", "TimeDomain", mergedTrainTestCols)
mergedTrainTestCols <- gsub("Acc", "Accelerometer", mergedTrainTestCols)
mergedTrainTestCols <- gsub("Gyro", "Gyroscope", mergedTrainTestCols)
mergedTrainTestCols <- gsub("Mag", "Magnitude", mergedTrainTestCols)
mergedTrainTestCols <- gsub("Freq", "Frequency", mergedTrainTestCols)
mergedTrainTestCols <- gsub("mean", "Mean", mergedTrainTestCols)
mergedTrainTestCols <- gsub("std", "StandardDeviation", mergedTrainTestCols)

mergedTrainTestCols <- gsub("BodyBody", "Body", mergedTrainTestCols)

# use new labels as column names

colnames(mergedTrainTest) <- mergedTrainTestCols


#####
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#####

# group by activity/subject and summarise using mean

#mergedTrainTestMeans <- mergedTrainTest[order(mergedTrainTest$subject, mergedTrainTest$activity),funs(mean)]
mergedTrainTestMeans <- aggregate(. ~subject+activity,mergedTrainTest,mean) 
mergedTrainTestMeans <- mergedTrainTestMeans[order(mergedTrainTestMeans$subject,mergedTrainTestMeans$activity),]


# output to file "tidy_data.txt"

write.table(mergedTrainTestMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)