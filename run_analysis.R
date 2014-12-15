# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#This script does the following:

#1.Merges the training and the test sets to create one data set.
X_temp1 <- read.table("UCI HAR Dataset/train/X_train.txt")
X_temp2 <- read.table("UCI HAR Dataset/test/X_test.txt")
X_data <- rbind(X_temp1,X_temp2)

Y_temp1 <- read.table("UCI HAR Dataset/train/Y_train.txt")
Y_temp2 <- read.table("UCI HAR Dataset/test/Y_test.txt")
Y_data <- rbind(Y_temp1,Y_temp2)

Sub_temp1 <- read.table("UCI HAR Dataset/train/subject_train.txt")
Sub_temp2 <- read.table("UCI HAR Dataset/test/subject_test.txt")
Sub_data <- rbind(Sub_temp1,Sub_temp2)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
Features <- read.table("UCI HAR Dataset/features.txt")
i_good_features <- grep("-mean\\(\\)|-std\\(\\)", Features[, 2])
X_data <- X_data[,i_good_features]
names(X_data) <- tolower(gsub("\\(|\\)","",Features[i_good_features,2]))

#3.Uses descriptive activity names to name the activities in the data set.
Activities <- read.table("UCI HAR Dataset/activity_labels.txt")
Activities[,2] <- gsub("_","",tolower(as.character(Activities[,2])))
Y_data[,1] <- Activities[Y_data[,1],2]
names(Y_data) <- "activity"

#4.Appropriately labels the data set with descriptive activity names.
names(Sub_data) <- "subject"
cleaned_data <- cbind(Sub_data, Y_data, X_data)
write.table(cleaned_data,"merged_and_cleaned.txt")

#5.Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniq_Sub <- unique(Sub_data)[,1]
num_Sub <- length(uniq_Sub)
num_Activities <- length(Activities[,1])
num_cols <- dim(cleaned_data)[2]

res <- cleaned_data[1:(num_Sub*num_Activities),]

row = 1
for (s in 1:num_Sub) {
        for (a in 1:num_Activities) {
                res[row, 1] = uniq_Sub[s]
                res[row, 2] = Activities[a, 2]
                tmp <- cleaned_data[cleaned_data$subject==s & cleaned_data$activity==Activities[a, 2], ]
                res[row, 3:num_cols] <- colMeans(tmp[, 3:num_cols])
                row = row+1
        }
}
write.table(res, "data_with_averages.txt",row.name=FALSE)