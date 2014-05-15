##Data selection rules
The following 3 files were merged for the two sets (train and test)

Train
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Activity ids.
- 'train/subject_train.txt': Subject id.

Test
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Activity ids.
- 'test/subject_test.txt': Subject id.

Each file was assigned a sequence id to join the 3 files in each set together.

Note: When loading the data from X_test.txt and X_train.txt only columns that contained std() or mean() were selected.
The result was 79 measures with associated activity ids and subject ids.  
Also, the column names were initially set to the labels included in the features.txt file

##Data Merge rules
The above test and train merged data set were combined into a single set

##Transform rules
The mean of the measures by subject and activity were computed to aggregate the result set

Finally the following rules were applied to the column names:

* Remove any repeating periods '.' - these were introduced automatically when R converted the feature labels from features.txt into compliant R names
* Remove any trailing periods
* Remove leading t and f


##Variables
This list shows the mapping from the original column name to the tidy data set column name.
Source	-> Target
* NA								-> activity_id
* NA								-> activityName
* NA								-> Group.1
* NA								-> Group.2
* NA								-> id
* NA								-> subject_id
* tBodyAcc-mean()-X					-> BodyAcc.mean.X
* tBodyAcc-mean()-Y					-> BodyAcc.mean.Y
* tBodyAcc-mean()-Z					-> BodyAcc.mean.Z
* tBodyAcc-std()-X					-> BodyAcc.std.X
* tBodyAcc-std()-Y					-> BodyAcc.std.Y
* tBodyAcc-std()-Z					-> BodyAcc.std.Z
* tGravityAcc-mean()-X				-> GravityAcc.mean.X
* tGravityAcc-mean()-Y				-> GravityAcc.mean.Y
* tGravityAcc-mean()-Z				-> GravityAcc.mean.Z
* tGravityAcc-std()-X				-> GravityAcc.std.X
* tGravityAcc-std()-Y				-> GravityAcc.std.Y
* tGravityAcc-std()-Z				-> GravityAcc.std.Z
* tBodyAccJerk-mean()-X				-> BodyAccJerk.mean.X
* tBodyAccJerk-mean()-Y				-> BodyAccJerk.mean.Y
* tBodyAccJerk-mean()-Z				-> BodyAccJerk.mean.Z
* tBodyAccJerk-std()-X				-> BodyAccJerk.std.X
* tBodyAccJerk-std()-Y				-> BodyAccJerk.std.Y
* tBodyAccJerk-std()-Z				-> BodyAccJerk.std.Z
* tBodyGyro-mean()-X				-> BodyGyro.mean.X
* tBodyGyro-mean()-Y				-> BodyGyro.mean.Y
* tBodyGyro-mean()-Z				-> BodyGyro.mean.Z
* tBodyGyro-std()-X					-> BodyGyro.std.X
* tBodyGyro-std()-Y					-> BodyGyro.std.Y
* tBodyGyro-std()-Z					-> BodyGyro.std.Z
* tBodyGyroJerk-mean()-X			-> BodyGyroJerk.mean.X
* tBodyGyroJerk-mean()-Y			-> BodyGyroJerk.mean.Y
* tBodyGyroJerk-mean()-Z			-> BodyGyroJerk.mean.Z
* tBodyGyroJerk-std()-X				-> BodyGyroJerk.std.X
* tBodyGyroJerk-std()-Y				-> BodyGyroJerk.std.Y
* tBodyGyroJerk-std()-Z				-> BodyGyroJerk.std.Z
* tBodyAccMag-mean()				-> BodyAccMag.mean
* tBodyAccMag-std()					-> BodyAccMag.std
* tGravityAccMag-mean()				-> GravityAccMag.mean
* tGravityAccMag-std()				-> GravityAccMag.std
* tBodyAccJerkMag-mean()			-> BodyAccJerkMag.mean
* tBodyAccJerkMag-std()				-> BodyAccJerkMag.std
* tBodyGyroMag-mean()				-> BodyGyroMag.mean
* tBodyGyroMag-std()				-> BodyGyroMag.std
* tBodyGyroJerkMag-mean()			-> BodyGyroJerkMag.mean
* tBodyGyroJerkMag-std()			-> BodyGyroJerkMag.std
* fBodyAcc-mean()-X					-> BodyAcc.mean.X.1
* fBodyAcc-mean()-Y					-> BodyAcc.mean.Y.1
* fBodyAcc-mean()-Z					-> BodyAcc.mean.Z.1
* fBodyAcc-std()-X					-> BodyAcc.std.X.1
* fBodyAcc-std()-Y					-> BodyAcc.std.Y.1
* fBodyAcc-std()-Z					-> BodyAcc.std.Z.1
* fBodyAcc-meanFreq()-X				-> BodyAcc.meanFreq.X
* fBodyAcc-meanFreq()-Y				-> BodyAcc.meanFreq.Y
* fBodyAcc-meanFreq()-Z				-> BodyAcc.meanFreq.Z
* fBodyAccJerk-mean()-X				-> BodyAccJerk.mean.X.1
* fBodyAccJerk-mean()-Y				-> BodyAccJerk.mean.Y.1
* fBodyAccJerk-mean()-Z				-> BodyAccJerk.mean.Z.1
* fBodyAccJerk-std()-X				-> BodyAccJerk.std.X.1
* fBodyAccJerk-std()-Y				-> BodyAccJerk.std.Y.1
* fBodyAccJerk-std()-Z				-> BodyAccJerk.std.Z.1
* fBodyAccJerk-meanFreq()-X			-> BodyAccJerk.meanFreq.X
* fBodyAccJerk-meanFreq()-Y			-> BodyAccJerk.meanFreq.Y
* fBodyAccJerk-meanFreq()-Z			-> BodyAccJerk.meanFreq.Z
* fBodyGyro-mean()-X				-> BodyGyro.mean.X.1
* fBodyGyro-mean()-Y				-> BodyGyro.mean.Y.1
* fBodyGyro-mean()-Z				-> BodyGyro.mean.Z.1
* fBodyGyro-std()-X					-> BodyGyro.std.X.1
* fBodyGyro-std()-Y					-> BodyGyro.std.Y.1
* fBodyGyro-std()-Z					-> BodyGyro.std.Z.1
* fBodyGyro-meanFreq()-X			-> BodyGyro.meanFreq.X
* fBodyGyro-meanFreq()-Y			-> BodyGyro.meanFreq.Y
* fBodyGyro-meanFreq()-Z			-> BodyGyro.meanFreq.Z
* fBodyAccMag-mean()				-> BodyAccMag.mean.1
* fBodyAccMag-std()					-> BodyAccMag.std.1
* fBodyAccMag-meanFreq()			-> BodyAccMag.meanFreq
* fBodyBodyAccJerkMag-mean()		-> BodyBodyAccJerkMag.mean
* fBodyBodyAccJerkMag-std()			-> BodyBodyAccJerkMag.std
* fBodyBodyAccJerkMag-meanFreq()	-> BodyBodyAccJerkMag.meanFreq
* fBodyBodyGyroMag-mean()			-> BodyBodyGyroMag.mean
* fBodyBodyGyroMag-std()			-> BodyBodyGyroMag.std
* fBodyBodyGyroMag-meanFreq()		-> BodyBodyGyroMag.meanFreq
* fBodyBodyGyroJerkMag-mean()		-> BodyBodyGyroJerkMag.mean
* fBodyBodyGyroJerkMag-std()		-> BodyBodyGyroJerkMag.std
* fBodyBodyGyroJerkMag-meanFreq()	-> BodyBodyGyroJerkMag.meanFreq
