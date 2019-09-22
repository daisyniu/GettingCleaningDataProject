library(dplyr)

## 1. download file and unzip the dataset

filename <- "dataset.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename,mode="wb")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


## 2. Merging the training and the test data sets
#2.1 reading files
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

feature<-read.table("./UCI HAR Dataset/features.txt")

activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

## naming columns

colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

## 2.2 Merging data into one set
one_train<-cbind(y_train,subject_train,x_train)
one_test<-cbind(y_test,subject_test,x_test)
OneSet<-rbind(one_train, one_test)

col_names<-colnames(OneSet)

##3. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std <- (grepl("activityId" , col_names) | 
                   grepl("subjectId" , col_names)|
                   grepl("mean..",col_names)|
                   grepl("std..",col_names)
                 )


data_mean_std<-OneSet[,mean_and_std ==TRUE]

##4.Uses descriptive activity names to name the activities in the data set 
TidySet<-merge(data_mean_std,activityLabels,by="activityId",All.x=TRUE)

##5 Appropriately labels the data set with descriptive variable names.
TidySet<-select(TidySet,-1)
names(TidySet)<-gsub("subjectId","subject", names(TidySet))
names(TidySet)<-gsub("activityType","activity", names(TidySet))
names(TidySet)<-gsub("Acc","Accelerometer", names(TidySet))
names(TidySet)<-gsub("-mean()","Mean", names(TidySet),ignore.case = TRUE)
names(TidySet)<-gsub("BodyBody","Body", names(TidySet))
names(TidySet)<-gsub("tBody","TimeBody", names(TidySet))
names(TidySet)<-gsub("^f","Frequency", names(TidySet))
names(TidySet)<-gsub("^t","Time", names(TidySet))
names(TidySet)<-gsub("-std()","STD", names(TidySet),ignore.case = TRUE)
names(TidySet)<-gsub("-freq()","Frequency", names(TidySet),ignore.case = TRUE)
names(TidySet)<-gsub("Gyro","Gyroscope", names(TidySet))
names(TidySet)<-gsub("Mag","Magnitude", names(TidySet))


##5 creates a second, independent tidy data set with the average of each variable for each activity and each subject.


SecTidySet <- TidySet %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

##6. write into a .txt file

write.table(SecTidySet, "secTidySet.txt", row.name=FALSE)
