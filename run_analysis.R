# Getting and Cleaning Data Project John Hopkins Coursera

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load Packages and get the Data
library(dplyr)
# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

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
## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-1,8

activities <- read.table(file.path(dataPath, "activity_labels.txt"))   # read activity labels

colnames(activities) <- c("activityId", "activityLabel")


	# Step 1 - Merge the training and the test sets to create one data set
	# concatenate individual data tables to make single data table.
	humanActivity <- rbind(
	  cbind(trainingSubjects, trainingValues, trainingActivity),
	  cbind(testSubjects, testValues, testActivity)
	)

rm(trainingSubjects, trainingValues, trainingActivity, # You can remove individual data tables to save memory.
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement.

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity)) # determine columns of data set to keep based on column name


							# ... and keep data in only the following columns. 
    humanActivity <- humanActivity[, columnsToKeep]


# Step 3 - Use descriptive activity names to name the activities in the data set.


humanActivity$activity <- factor(humanActivity$activity, # replace activity values with named factor levels

                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names


humanActivityCols <- colnames(humanActivity) # get column names



humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)  # remove special characters

	# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols



# Step 5 - Create a second, independent tidy set with the average of each
#     variable for each activity and each subject

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean)) ## Summarise_each is is a little old but works fine and gives the desired output!
# output to file "TidyData.txt"
write.table(humanActivityMeans, "TidyData.txt", row.names = FALSE, 
            quote = FALSE)
