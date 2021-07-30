# Loading packages
library(tidyverse)


# Download data
link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(link, "Coursera_DS3_Final.zip", "libcurl")
unzip("Coursera_DS3_Final.zip")

# Assigning all data frames

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# Step 1: Merges the training and the test sets to create one data set.

setequal(names(x_test), names(x_train))
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
data_combine <- cbind(Subject, y, x)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

data <- data_combine %>% 
        select(subject, code, contains("mean"), contains("std"))

# Step 3: Uses descriptive activity names to name the activities in the data set.

data$code <- activities[data$code, 2]

# Step 4: Appropriately labels the data set with descriptive variable names.

names(data)[2] = "activity"
names(data) <- gsub("Acc", "Accelerometer", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("^t", "Time", names(data))
names(data) <- gsub("^f", "Frequency", names(data))
names(data) <- gsub("tBody", "TimeBody", names(data))
names(data) <- gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
names(data) <- gsub("-std()", "STD", names(data), ignore.case = TRUE)
names(data) <- gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
names(data) <- gsub("angle", "Angle", names(data))
names(data) <- gsub("gravity", "Gravity", names(data))

# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

write.table(tidydata, "tidydata.txt", row.name = FALSE)
