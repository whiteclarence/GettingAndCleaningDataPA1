run_Analysis <- function() {
	
	#1.Merges the training and the test sets to create one data set.
	library(memisc)
	setwd("UCI HAR Dataset")
	activity_labels <- read.table("activity_labels.txt")
	features <- read.table("features.txt")
	setwd("test")
	x_test <- read.table("X_test.txt")
	colnames(x_test) <- features$V2
	subject_test <- read.table("subject_test.txt")
	y_test <- read.table("y_test.txt")
	y_test <- merge(y_test, activity_labels, by.x = "V1", by.y = "V1", sort = FALSE)
	y_test <- y_test[,2]
	test <- cbind(x_test, subject_test, y_test)
	colnames(test)[colnames(test) =="V1"] <- "Subject"
	colnames(test)[colnames(test) =="y_test"] <- "Activity"
	setwd("..")
	setwd("train")
	x_train <- read.table("X_train.txt")
	colnames(x_train) <- features$V2
	subject_train <- read.table("subject_train.txt")
	y_train <- read.table("y_train.txt")
	y_train <- merge(y_train, activity_labels, by.x = "V1", by.y = "V1", sort = FALSE)
	y_train <- y_train[,2]
	train <- cbind(x_train, subject_train, y_train)
	colnames(train)[colnames(train) =="V1"] <- "Subject"
	colnames(train)[colnames(train) =="y_train"] <- "Activity"
	setwd("..")
	setwd("..")
	combined_dataset <- rbind(test, train)
	
	#2.Extracts only the measurements on the mean and standard deviation for each measurement.

	combined_dataset <- combined_dataset[,c(562:563, 1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)]
	uniqueSubjects <- unique(combined_dataset$Subject)
	numSubjects <- length(unique(combined_dataset$Subject))
	numActivities <- length(activity_labels[,1])
	numCol <- dim(combined_dataset)[2]
	tidy_dataset <- data.set(combined_dataset[1:(numSubjects * numActivities),])
	colnames(tidy_dataset) <- colnames(combined_dataset)

	#3.Uses descriptive activity names to name the activities in the data set

	row = 1
	for (i in 1:numSubjects){
		for(j in 1:numActivities){
			tidy_dataset[row, 1] = uniqueSubjects[i]
			tidy_dataset[row, 2] = activity_labels[j, 2]
			tmp <- combined_dataset[combined_dataset$Subject==i & combined_dataset$Activity==activity_labels[j, 2],]
			tidy_dataset[row, 3:numCol] <- colMeans(tmp[, 3:numCol])
			row=row+1
		}
	}
	tidy_dataset <- tidy_dataset[complete.cases(tidy_dataset),]
	
	##4.Appropriately labels the data set with descriptive variable names.
	##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
	
	tidy_dataset <- within(tidy_dataset, {
		description(Subject) <- "Subject # that performed activity"
		description(Activity) <- "Activity performed by the subject"
	 	
		description(`tBodyAcc-mean()-X`) <- "Mean of the X time domain axial signal measurement of the Body Acceleration"
	 	description(`tBodyAcc-mean()-Y`) <- "Mean of the Y time domain axial signal measurement of the Body Acceleration"
		description(`tBodyAcc-mean()-Z`) <- "Mean of the Z time domain axial signal measurement of the Body Acceleration"
		description(`tBodyAcc-std()-X`) <- "Standard Deviation of the X time domain axial signal measurement of the Body Acceleration"
		description(`tBodyAcc-std()-Y`) <- "Standard Deviation of the Y time domain axial signal measurement of the Body Acceleration"
		description(`tBodyAcc-std()-Z`) <- "Standard Deviation of the Z time domain axial signal measurement of the Body Acceleration"
		description(`tGravityAcc-mean()-X`) <- "Mean of the X time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAcc-mean()-Y`) <- "Mean of the Y time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAcc-mean()-Z`) <- "Mean of the Z time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAcc-std()-X`) <- "Standard Deviation of the X time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAcc-std()-Y`) <- "Standard Deviation of the Y time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAcc-std()-Z`) <- "Standard Deviation of the Z time domain axial signal measurement of the Gravity Acceleration"
		description(`tBodyAccJerk-mean()-X`) <- "Mean of the X time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerk-mean()-Y`) <- "Mean of the Y time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerk-mean()-Z`) <- "Mean of the Z time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerk-std()-X`) <- "Standard Deviation of the X time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerk-std()-Y`) <- "Standard Deviation of the Y time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerk-std()-Z`) <- "Standard Deviation of the Z time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyGyro-mean()-X`) <- "Mean of the X time domain axial signal measurement of the Body Gyroscope"
	 	description(`tBodyGyro-mean()-Y`) <- "Mean of the Y time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyro-mean()-Z`) <- "Mean of the Z time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyro-std()-X`) <- "Standard Deviation of the X time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyro-std()-Y`) <- "Standard Deviation of the Y time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyro-std()-Z`) <- "Standard Deviation of the Z time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-mean()-X`) <- "Mean of the X time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-mean()-Y`) <- "Mean of the Y time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-mean()-Z`) <- "Mean of the Z time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-std()-X`) <- "Standard Deviation of the X time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-std()-Y`) <- "Standard Deviation of the Y time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerk-std()-Z`) <- "Standard Deviation of the Z time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyAccMag-mean()`) <- "Mean of the Magnitude of the time domain axial signal measurement of the Body Acceleration"
	 	description(`tBodyAccMag-std()`) <- "Standard Deviation of the Magnitude of the time domain axial signal measurement of the Body Acceleration"
		description(`tGravityAccMag-mean()`) <- "Mean of the Magnitude of the time domain axial signal measurement of the Gravity Acceleration"
		description(`tGravityAccMag-std()`) <- "Standard Deviation of the Magnitude of the time domain axial signal measurement of the Gravity Acceleration"
		description(`tBodyAccJerkMag-mean()`) <- "Mean of the Magnitude of the time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyAccJerkMag-std()`) <- "Standard Deviation of the Magnitude of the time domain jerk signal measurement of the Body Acceleration"
		description(`tBodyGyroMag-mean()`) <- "Mean of the Magnitude of the time domain axial signal measurement of the Body Gyroscope"
	 	description(`tBodyGyroMag-std()`) <- "Standard Deviation of the Magnitude of the time domain axial signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerkMag-mean()`) <- "Mean of the Magnitude of the time domain jerk signal measurement of the Body Gyroscope"
		description(`tBodyGyroJerkMag-std()`) <- "Standard Deviation of the Magnitude of the time domain jerk signal measurement of the Body Gyroscope"
		description(`fBodyAcc-mean()-X`) <- "Mean of the X frequency domain axial signal measurement of the Body Acceleration"
	 	description(`fBodyAcc-mean()-Y`) <- "Mean of the Y frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyAcc-mean()-Z`) <- "Mean of the Z frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyAcc-std()-X`) <- "Standard Deviation of the X frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyAcc-std()-Y`) <- "Standard Deviation of the Y frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyAcc-std()-Z`) <- "Standard Deviation of the Z frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-mean()-X`) <- "Mean of the X frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-mean()-Y`) <- "Mean of the Y frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-mean()-Z`) <- "Mean of the Z frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-std()-X`) <- "Standard Deviation of the X frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-std()-Y`) <- "Standard Deviation of the Y frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyAccJerk-std()-Z`) <- "Standard Deviation of the Z frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyGyro-mean()-X`) <- "Mean of the X frequency domain axial signal measurement of the Body Gyroscope"
	 	description(`fBodyGyro-mean()-Y`) <- "Mean of the Y frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyGyro-mean()-Z`) <- "Mean of the Z frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyGyro-std()-X`) <- "Standard Deviation of the X frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyGyro-std()-Y`) <- "Standard Deviation of the Y frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyGyro-std()-Z`) <- "Standard Deviation of the Z frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyAccMag-mean()`) <- "Mean of the Magnitude of the frequency domain axial signal measurement of the Body Acceleration"
	 	description(`fBodyAccMag-std()`) <- "Standard Deviation of the Magnitude of the frequency domain axial signal measurement of the Body Acceleration"
		description(`fBodyBodyAccJerkMag-mean()`) <- "Mean of the Magnitude of the frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyBodyAccJerkMag-std()`) <- "Standard Deviation of the Magnitude of the frequency domain jerk signal measurement of the Body Acceleration"
		description(`fBodyBodyGyroMag-mean()`) <- "Mean of the Magnitude of the frequency domain axial signal measurement of the Body Gyroscope"
	 	description(`fBodyBodyGyroMag-std()`) <- "Standard Deviation of the Magnitude of the frequency domain axial signal measurement of the Body Gyroscope"
		description(`fBodyBodyGyroJerkMag-mean()`) <- "Mean of the Magnitude of the frequency domain jerk signal measurement of the Body Gyroscope"
		description(`fBodyBodyGyroJerkMag-std()`) <- "Standard Deviation of the Magnitude of the frequency domain jerk signal measurement of the Body Gyroscope"
	
		labels(Subject) <- c("Subject 1 used for training" = 1,
				"Subject 2 used for testing" = 2,
				"Subject 3 used for training" = 3,
				"Subject 4 used for testing" = 4,
				"Subject 5 used for training" = 5,
				"Subject 6 used for training" = 6,
				"Subject 7 used for training" = 7,
				"Subject 8 used for training" = 8,
				"Subject 9 used for testing" = 9,
				"Subject 10 used for testing" = 10,
				"Subject 11 used for training" = 11,
				"Subject 12 used for testing" = 12,
				"Subject 13 used for testing" = 13,
				"Subject 14 used for training" = 14,
				"Subject 15 used for training" = 15,
				"Subject 16 used for training" = 16,
				"Subject 17 used for training" = 17,
				"Subject 18 used for testing" = 18,
				"Subject 19 used for training" = 19,
				"Subject 20 used for testing" = 20,
				"Subject 21 used for training" = 21,
				"Subject 22 used for training" = 22,
				"Subject 23 used for training" = 23,
				"Subject 24 used for testing" = 24,
				"Subject 25 used for training" = 25,
				"Subject 26 used for training" = 26,
				"Subject 27 used for training" = 27,
				"Subject 28 used for training" = 28,
				"Subject 29 used for training" = 29,
				"Subject 30 used for training" = 30)
	})

	write.table(tidy_dataset, "tidy_dataset.txt", sep="\t", row.name=FALSE)
	Write(codebook(tidy_dataset), file="codebook.md")
}
