

library(dplyr)

## read train data
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## read test data
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## read data description
variable_names <- read.table("./UCI HAR Dataset/features.txt")

## read activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## 1. Merges the training and the test sets to create one data set.
Xtotal <- rbind(Xtrain, Xtest)
Ytotal <- rbind(Ytrain, Ytest)
Sub_total <- rbind(Sub_train, Sub_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
Xtotal <- Xtotal[,selected_var[,1]]

## 3. Uses descriptive activity names to name the activities in the data set
colnames(Ytotal) <- "activity"
Ytotal$activitylabel <- factor(Ytotal$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Ytotal[,-1]

## 4. Appropriately labels the data set with descriptive variable names.
colnames(Xtotal) <- variable_names[selected_var[,1],2]

## 5. From the data set in step 4, creates a second, independent tidy data set with the average
## of each variable for each activity and each subject.

colnames(Sub_total) <- "subject"
total <- cbind(Xtotal, activitylabel, Sub_total)
total_mean <- total %>% 
  group_by(activitylabel, subject) %>% 
  summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
write.table(total_mean, file = "tidydata.txt", row.names = FALSE, col.names = TRUE) # or
