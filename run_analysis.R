library(data.table)
library(dplyr)
if(!file.exists("./data")){dir.create("./data")}
download.file(url ="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="./data/Dataset.zip")
##Unzip the File
unzip(zipfile="./data/Dataset.zip",exdir="./data")
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files
##Reading the data
dataactivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)
datasubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
datafeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)
dataactivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
datasubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)
datafeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
str(dataactivityTrain)
str(datasubjectTrain)
str(datafeaturesTrain)
str(dataactivityTest)
str(datasubjectTest)
str(datafeaturesTest)
##1. Merges the training and the test sets to create one data set.
datasubject <- rbind(datasubjectTrain, datasubjectTest)
dataactivity <- rbind(dataactivityTrain, dataactivityTest)
datafeatures <- rbind(datafeaturesTrain, datafeaturesTest)
names(datasubject)<-c("subject")
names(dataactivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(datafeatures)<- dataFeaturesNames$V2
completeData <- cbind(datafeatures,dataactivity,datasubject)
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
selectedNames<-c(as.character(columnsWithMeanSTD), "subject", "activity" )
completeData<-subset(completeData,select=selectedNames)
str(completeData)
##3. Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
completeData$activity <- as.character(completeData$activity)
for (i in 1:6){
  completeData$activity[completeData$activity == i] <- as.character(activityLabels[i,2])
}
completeData$activity <- as.factor(completeData$activity)
head(completeData$activity,30)
##4. Appropriately labels the data set with descriptive variable names
names(completeData)
names(completeData)<-gsub("Acc", "Accelerometer", names(completeData))
names(completeData)<-gsub("Gyro", "Gyroscope", names(completeData))
names(completeData)<-gsub("BodyBody", "Body", names(completeData))
names(completeData)<-gsub("Mag", "Magnitude", names(completeData))
names(completeData)<-gsub("^t", "Time", names(completeData))
names(completeData)<-gsub("^f", "Frequency", names(completeData))
names(completeData)<-gsub("tBody", "TimeBody", names(completeData))
names(completeData)<-gsub("-mean()", "Mean", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("-std()", "STD", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("-freq()", "Frequency", names(completeData), ignore.case = TRUE)
names(completeData)<-gsub("angle", "Angle", names(completeData))
names(completeData)<-gsub("gravity", "Gravity", names(completeData))
names(completeData)
write.table(completeData, file = "completedata_4.txt", row.names = FALSE)
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
completeData$subject <- as.factor(completeData$subject)
completeData <- data.table(completeData)
tidyData <- aggregate(. ~subject + activity, completeData, mean)
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "Tidydata.txt", row.names = FALSE)
