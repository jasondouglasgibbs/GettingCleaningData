## Getting and Cleaning Data Peer Graded Assignment - Jason Gibbs##
##run_analysis.R file that:
  #1. Merges the training and the test sets to create one data set.
  #2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  #3. Uses descriptive activity names to name the activities in the data set
  #4. Appropriately labels the data set with descriptive variable names. 
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Libraries#
library(tidyverse)

#Set working directory#
setwd("C:\\Users\\fight\\Documents\\R\\GettingCleaningData\\")

#Load Data#
features <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("code", "activity"))
subjecttest <- read.table("UCI HAR Dataset\\test\\subject_test.txt", col.names = "subject")
subjecttrain <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = "subject")
xtest <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = features$functions)
ytest <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "code")
xtrain <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = features$functions)
ytrain <- read.table("UCI HAR Dataset\\train\\y_train.txt", col.names = "code")


#1. Merges the training and the test sets to create one data set.
  #Uses rbind and cbind commands to merge the data sets.
XBind<-rbind(xtest,xtrain)
YBind<-rbind(ytest,ytrain)
SubjectBind<-rbind(subjecttest,subjecttrain)
SingleData<-cbind(XBind,YBind,SubjectBind)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
ExtractedData <- SingleData %>% select(subject, code, contains("mean"), contains("std"))

#3. Uses descriptive activity names to name the activities in the data set
  #Creates a new column and uses the recode function to provide the descriptive activity name.
RecodedData<-ExtractedData
AddColumn<-ExtractedData$code
RecodedData$activity<-AddColumn
RecodedData<-RecodedData %>% mutate(activity=recode(activity,'1'="Walking",'2'="Walking Upstairs",'3'="Walking Downstairs",'4'="Sitting",'5'="Standing",'6'="Laying"))
RecodedData<-RecodedData %>% select(subject, code, activity, everything())

#4. Appropriately labels the data set with descriptive variable names.
  #Saves the column names in the DF and then performs character substitutions#
ColumnNames<-names(RecodedData)
names(RecodedData)<-gsub("Acc", "Accelerometer", names(RecodedData))
names(RecodedData)<-gsub("Gyro", "Gyroscope", names(RecodedData))
names(RecodedData)<-gsub("BodyBody", "Body", names(RecodedData))
names(RecodedData)<-gsub("Mag", "Magnitude", names(RecodedData))
names(RecodedData)<-gsub("^t", "Time", names(RecodedData))
names(RecodedData)<-gsub("^f", "Frequency", names(RecodedData))
names(RecodedData)<-gsub("tBody", "TimeBody", names(RecodedData))
names(RecodedData)<-gsub("-mean()", "Mean", names(RecodedData), ignore.case = TRUE)
names(RecodedData)<-gsub("-std()", "STD", names(RecodedData), ignore.case = TRUE)
names(RecodedData)<-gsub("-freq()", "Frequency", names(RecodedData), ignore.case = TRUE)
names(RecodedData)<-gsub("angle", "Angle", names(RecodedData))
names(RecodedData)<-gsub("gravity", "Gravity", names(RecodedData))
ColumnNames<-names(RecodedData)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  #Uses summarize function to provide the mean by subject and activity, then writes a .csv with that information.
AverageData<-RecodedData %>% group_by(subject, activity)  %>% summarize_all(mean)
write.table(AverageData,"AverageDataSet.txt", row.name=FALSE)
