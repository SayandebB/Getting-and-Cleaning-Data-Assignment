run_analysis <- function()
{
# Download raw data and save in the folder "download/data"
	if(!file.exists("./download")){dir.create("./download")}
	fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(fileurl, destfile = "./download/data.zip")

# Unzip the folder, by default it is stored in the working directory
	unzip("./download/data.zip")

# Creating a variable to hold the reference file path
	refPath <- "UCI HAR Dataset"

# The relevant data is around 3 variables each of "test" and "train"
# The variables are:
# Test activities: data in "./test/Y_test.txt". This data is to be read in data table "activityTest"
# Training activities: data in "./train/Y_train.txt". This data is to be read in data table "activityTrain"
# Test subjects: data in "./test/subject_test.txt". This data is to be read in data table "subjectTest"
# Training subjects: data in "./test/subject_train.txt". This data is to be read in data table "subjectTrain"
# Test features: data in "./test/X_test.txt". This data is to be read in data table "featuresTest"
# Train features: data in "./train/X_train.txt". This data is to be read in data table "featuresTrain"

# Read the above data in separate files...

	activityTest <- read.table(file.path(refPath, "test", "Y_test.txt" ), header = FALSE)
	activityTrain <- read.table(file.path(refPath, "train", "Y_train.txt" ), header = FALSE)

	subjectTest <- read.table(file.path(refPath, "test" , "subject_test.txt" ),header = FALSE)
	subjectTrain <- read.table(file.path(refPath, "train" , "subject_train.txt" ),header = FALSE)

	featuresTest <- read.table(file.path(refPath, "test" , "X_test.txt" ),header = FALSE)
	featuresTrain <- read.table(file.path(refPath, "train", "X_train.txt"), header = FALSE)

# ASSIGNMENT 1: Merges training and test data into "MergedData"
# Merge training and test data by variable
	subject <- rbind(subjectTrain, subjectTest)
	activity <- rbind(activityTrain, activityTest)
	features <- rbind(featuresTrain, featuresTest)

# Give names to the table headers
	names(subject) <- c("subject")
	names(activity) <- c("activity")
	NamesOfFeatures <- read.table(file.path(refPath, "features.txt"), head=FALSE)
	names(features) <- NamesOfFeatures[,2]

# Create the merged data set
	MergedData.temp <-cbind(subject, activity)
	MergedData <- cbind(features, MergedData.temp)

# ASSIGNMENT 2: Extracts only the measurements on the mean and standard deviation for each measurement
	# Create a vector to hold column names of merged data
	colVec <- colnames(MergedData) 

	# Extract column names with 'mean()' and 'std()'
	subVecMeanStd <- grep("mean\\(\\)|std\\(\\)", colVec, value=T)

	# Add 'subject' and 'activity' to the column names and hold it in a separate variable 'finalColNames'
	finalColNames <- c(subVecMeanStd, "subject", "activity")

	# Subset merged data with 'finalColNames'
	DataMeanStd <- subset(MergedData, select = finalColNames)
	
# ASSIGNMENT 3: Uses descriptive activity names to name the activities in the data set
	DataMeanStd$activity <- as.character(DataMeanStd$activity)

	DataMeanStd$activity[DataMeanStd$activity == 1] <- "Walking"
	DataMeanStd$activity[DataMeanStd$activity == 2] <- "Walking Upstairs"
	DataMeanStd$activity[DataMeanStd$activity == 3] <- "Walking Downstairs"
	DataMeanStd$activity[DataMeanStd$activity == 4] <- "Sitting"
	DataMeanStd$activity[DataMeanStd$activity == 5] <- "Standing"
	DataMeanStd$activity[DataMeanStd$activity == 6] <- "Laying"

	DataMeanStd$activity <- as.factor(DataMeanStd$activity)

# ASSIGNMENT 4: Appropriately labels the data set with descriptive variable names.  
	
	names(DataMeanStd)<-gsub("^t", "time", names(DataMeanStd))
	names(DataMeanStd)<-gsub("^f", "frequency", names(DataMeanStd))
	names(DataMeanStd)<-gsub("Acc", "Accelerometer", names(DataMeanStd))
	names(DataMeanStd)<-gsub("Gyro", "Gyroscope", names(DataMeanStd))
	names(DataMeanStd)<-gsub("Mag", "Magnitude", names(DataMeanStd))
	names(DataMeanStd)<-gsub("BodyBody", "Body", names(DataMeanStd))

# ASSIGNMENT 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
	
	# Call the relevant libraries
	library(data.table)
	library(plyr)
	library(dplyr)
	
	# Find mean by subject and activity
	DataMeanStd_dt <- data.table(DataMeanStd)
	TidyData_sub <- DataMeanStd_dt[, lapply(.SD, mean), by = 'subject']
	TidyData_act <- DataMeanStd_dt[, lapply(.SD, mean), by = 'activity']
	TidyData <- rbind(TidyData_act, TidyData_sub)

	# Write data table to local file
	write.table(TidyData, file = "TidyData.txt", row.names = FALSE)

# END
}

