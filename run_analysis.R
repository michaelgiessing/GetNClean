## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
library(magrittr)

#setwd('C:/Users/michael.giessing/Box Sync/Michael Giessing/Courses/Coursera/GetNCleanData/Project')

#Download and unzip
#download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip','FUCI_HAR_Dataset.zip')

unzip('FUCI_HAR_Dataset.zip')


#Load activity labels
activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt')
names(activity_labels) = c("ActivityID", "ActivityDSC")

# Load: data column names and make filtered list with mean or std in the name
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
goodfeatures <- grepl("mean|std",features)

##Loading Test Data
#Read in test data
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
X_test <-  read.table('./UCI HAR Dataset/test/X_test.txt')
Y_test <-  read.table('./UCI HAR Dataset/test/Y_test.txt')

#Name the variables in X_test and subject_test
names(X_test) <- features
names(subject_test) <- "SubjectID"

#Get the features that have mean or std in the name
X_test <- X_test[,goodfeatures]

#Join activity labels on to y_test
names(Y_test) = "ActivityID"
Y_test <- inner_join(Y_test,activity_labels)

#Bind it all together
test_data <- cbind(subject_test,Y_test,X_test)

##Loading Train Data
#Read in Train data
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
X_train <-  read.table('./UCI HAR Dataset/train/X_train.txt')
Y_train <-  read.table('./UCI HAR Dataset/train/Y_train.txt')

#Name the variables in X_train and subject_train
names(X_train) <- features
names(subject_train) <- "SubjectID"

#Get the features that have mean or std in the name
X_train <- X_train[,goodfeatures]

#Join activity labels on to y_train
names(Y_train) = "ActivityID"
Y_train <- inner_join(Y_train,activity_labels)

#Bind it all together
train_data <- cbind(subject_train,Y_train,X_train)

# Merge test and train data
data = rbind(test_data, train_data)


#Create functions for name substitution
timefort <- function (x){
  timefort <- sub("^t","Time",x)
}
frequencyforf <- function (x){
  frequencyforf <- sub("^f","Freqency",x)
}
AccelerationforAcc <- function (x){
  AccelerationforAcc <- gsub("Acc","Acceleration",x)
}
GyroscopeforGyro <- function (x){
  AccelerationforAcc <- gsub("Gyro","Gyroscop",x)
}
MagnitudeforMag <- function (x){
  AccelerationforAcc <- gsub("Mag","Magnitude",x)
}
StandardDeviationforstd <- function (x){
  standardDeviationforstd <- gsub("std","StandardDeviation",x)
}
FrequencyforFreq <- function (x){
  FrequencyforFreq <- gsub("Freq","Frequency",x)
}
Meanformean <- function (x){
  Meanformean <- gsub("mean()","Mean",x)
}
RemoveChar <- function (x){
  y<- gsub("\\(\\)","",x)
  RemoveChar <- gsub("-","",y)
}

#Fix variable names
x<- names(data)
x<-sapply(x,timefort)
x<-sapply(x,FrequencyforFreq)
x<-sapply(x,frequencyforf)
x<-sapply(x,AccelerationforAcc)
x<-sapply(x,GyroscopeforGyro)
x<-sapply(x,MagnitudeforMag)
x<-sapply(x,StandardDeviationforstd)
x<-sapply(x,Meanformean)
x<-sapply(x,RemoveChar)
names(data)<-x

#Get mean for each variable for each group
#Result Stared in SummaryTBL
SummaryTBL <- data %>%
select(-ActivityID) %>%
group_by(ActivityDSC, SubjectID) %>%
summarize_all(mean)

write.table(SummaryTBL, file = "./independent_tidy_data.txt", row.name = FALSE)
