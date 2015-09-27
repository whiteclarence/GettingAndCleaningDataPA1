Readme file for tidy_dataset

I. Test dataset
1. activity_labels = the labels as defined in the activity_labels.txt file
2. features = the features from the features.txt file.  Used to define column names
3. x_test = data from the x_test.txt file.  Column names are set to variables in column two of features
4. subject_test = the data defined in the subject_test.txt file
5. y_test = data from the y_test.txt file.  activity_labels are added.
6. test = Columns of x_test and y_test are binded.  Subject and Activity column names are added.

II. Train dataset
1. activity_labels = the labels as defined in the activity_labels.txt file
2. features = the features from the features.txt file.  Used to define column names
3. x_train = data from the x_train.txt file.  Column names are set to variables in column two of features
4. subject_test = the data defined in the subject_test.txt file
5. y_train = data from the y_train.txt file.  activity_labels are added.
6. train = Columns of x_train and y_train are binded.  Subject and Activity column names are added.

III. Combined dataset
1. combined_dataset = the combined rows of test and train.
2. uniqueSubjects = list of subject numbers in the dataset
3. numSubjects = total number of subjects in dataset
4. numActivities = total number of activities in dataset
5. numCol = number of columns in combined_dataset

IV. Tidy dataset
1. To make the tidy_dataset we loop through the combined_dataset and take the Column mean 
of each activity for each subject. 
2. After looping we remove rows with missing values due to subject not performing said activity.

V.  Ouput
After the dataset is created we create a description for each column and labels for the subjects.
We then write the dataset and codebooks to a file.
