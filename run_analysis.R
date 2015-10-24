## Course Project - Getting and Cleaning Data

## Dowload and unzip file

if(!file.exists("~/data")) {dir.create("~/data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "~/data/database.zip")
setwd("~/data")
unzip("database.zip")


## Load tables in parent folder

setwd("~/data/UCI HAR Dataset")

features <- read.table("features.txt")

activities <- read.table("activity_labels.txt")
names(activities) <- c("activity_index","activity_label")


## Load tables from train folder

setwd("~/data/UCI HAR Dataset/train")

X_train <- read.table("X_train.txt")
Y_train <- read.table("Y_train.txt")
subject_train <- read.table("subject_train.txt")

train <- cbind(subject_train,Y_train,X_train)


## Load tables from test folder

setwd("~/data/UCI HAR Dataset/test")

X_test <- read.table("X_test.txt")
Y_test <- read.table("Y_test.txt")
subject_test <- read.table("subject_test.txt")

test <- cbind(subject_test,Y_test,X_test)


## 1 - Merge the training and the test sets to create one data set

total <- rbind(train,test)


## 4 - Appropriately label the data set with descriptive variable names

features <- as.vector(features$V2)
features <- append(features, c("Subject","Activity"), after = 0)
names(total) <- features


## 2 - Extract only the measurements on the mean and standard deviation for each measurement.

mean_and_std <- total[,which(grepl("mean()",names(total)) | grepl("std()",names(total)))]
mean_and_std <- mean_and_std[,-which(grepl("-meanFreq()",names(mean_and_std)))]
new_total <- cbind(Subject = total$Subject,Activity = total$Activity,mean_and_std)


## 3 - Use descriptive activity names to name the activities in the data set.

for (i in 1:length(new_total$Activity)) {
  if (new_total$Activity[i] %in% activities$activity_index){
    new_total$Activity[i] <- as.character(activities[which(new_total$Activity[i] == activities$activity_index),]$activity_label)
  } else {
    stop("activity index not found")
  }
}


## 5 - From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

new_total$Subject <- as.factor(new_total$Subject)
new_total$Activity <- as.factor(new_total$Activity)

tidy <- aggregate(new_total[,-(1:2)],list(Subject = new_total$Subject, Activity = new_total$Activity), mean, na.rm = TRUE)


## Write a text file with tidy dataset

setwd("~/data")
write.table(tidy, file="tidy.txt", row.names=FALSE)