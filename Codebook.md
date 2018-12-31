# Code Book
This is a code book that describes the variables, data and any transformations or work that you performed to clean up the data.

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone on the waist.

| Variable name | Description |
| ----- | ----- |
| subject | ID number of 30 volunteers who performed the activity. The valid range is from 1 to 30. |
| activity | Activity type performed |

| Value | Value label |
| ----- | ----- |
| 1 | Walking |
| 2 | Walking upstairs |
| 3 | Walking downstairs |
| 4 | Sitting |
| 5 | Standing |
| 6 | Laying |

Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals timeAccelerometer-XYZ and timeGyroscope-XYZ. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (timeBodyAccelerometer-XYZ and timeGravityAccelerometer-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (timeBodyAccelerometerJerk-XYZ and timeBodyGyroscopeJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (timeBodyAccelerometerMagnitude, timeGravityAccelerometerMagnitude, timeBodyAccelerometerJerkMagnitude, timeBodyGyroscopeMagnitude, timeBodyGyroscopeJerkMagnitude). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing frequenceBodyAccelerometer-XYZ, frequencyBodyAccelerometerJerk-XYZ, frequencyBodyGyroscope-XYZ, frequenceBodyAcclerometerJerkMagnitude, frequenceBodyGyroscopeMagnitude, frequencyBodyGyroscopeJerkMagnitude. 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

```
timeBodyAccelerometer-mean()-X
timeBodyAccelerometer-mean()-Y
timeBodyAccelerometer-mean()-Z
timeBodyAccelerometer-std()-X
timeBodyAccelerometer-std()-Y
timeBodyAccelerometer-std()-Z
timeGravityAccelerometer-mean()-X
timeGravityAccelerometer-mean()-Y
timeGravityAccelerometer-mean()-Z
timeGravityAccelerometer-std()-X
timeGravityAccelerometer-std()-Y
timeGravityAccelerometer-std()-Z
timeBodyAccelerometerJerk-mean()-X
timeBodyAccelerometerJerk-mean()-Y
timeBodyAccelerometerJerk-mean()-Z
timeBodyAccelerometerJerk-std()-X
timeBodyAccelerometerJerk-std()-Y
timeBodyAccelerometerJerk-std()-Z
timeBodyGyroscope-mean()-X
timeBodyGyroscope-mean()-Y
timeBodyGyroscope-mean()-Z
timeBodyGyroscope-std()-X
timeBodyGyroscope-std()-Y
timeBodyGyroscope-std()-Z
timeBodyGyroscopeJerk-mean()-X
timeBodyGyroscopeJerk-mean()-Y
timeBodyGyroscopeJerk-mean()-Z
timeBodyGyroscopeJerk-std()-X
timeBodyGyroscopeJerk-std()-Y
timeBodyGyroscopeJerk-std()-Z
timeBodyAccelerometerMagnitude-mean()
timeBodyAccelerometerMagnitude-std()
timeGravityAccelerometerMagnitude-mean()
timeGravityAccelerometerMagnitude-std()
timeBodyAccelerometerJerkMagnitude-mean()
timeBodyAccelerometerJerkMagnitude-std()
timeBodyGyroscopeMagnitude-mean()
timeBodyGyroscopeMagnitude-std()
timeBodyGyroscopeJerkMagnitude-mean()
timeBodyGyroscopeJerkMagnitude-std()
frequencyBodyAccelerometer-mean()-X
frequencyBodyAccelerometer-mean()-Y
frequencyBodyAccelerometer-mean()-Z
frequencyBodyAccelerometer-std()-X
frequencyBodyAccelerometer-std()-Y
frequencyBodyAccelerometer-std()-Z
frequencyBodyAccelerometer-meanFreq()-X
frequencyBodyAccelerometer-meanFreq()-Y
frequencyBodyAccelerometer-meanFreq()-Z
frequencyBodyAccelerometerJerk-mean()-X
frequencyBodyAccelerometerJerk-mean()-Y
frequencyBodyAccelerometerJerk-mean()-Z
frequencyBodyAccelerometerJerk-std()-X
frequencyBodyAccelerometerJerk-std()-Y
frequencyBodyAccelerometerJerk-std()-Z
frequencyBodyAccelerometerJerk-meanFreq()-X
frequencyBodyAccelerometerJerk-meanFreq()-Y
frequencyBodyAccelerometerJerk-meanFreq()-Z
frequencyBodyGyroscope-mean()-X
frequencyBodyGyroscope-mean()-Y
frequencyBodyGyroscope-mean()-Z
frequencyBodyGyroscope-std()-X
frequencyBodyGyroscope-std()-Y
frequencyBodyGyroscope-std()-Z
frequencyBodyGyroscope-meanFreq()-X
frequencyBodyGyroscope-meanFreq()-Y
frequencyBodyGyroscope-meanFreq()-Z
frequencyBodyAccelerometerMagnitude-mean()
frequencyBodyAccelerometerMagnitude-std()
frequencyBodyAccelerometerMagnitude-meanFreq()
frequencyBodyAccelerometerJerkMagnitude-mean()
frequencyBodyAccelerometerJerkMagnitude-std()
frequencyBodyAccelerometerJerkMagnitude-meanFreq()
frequencyBodyGyroscopeMagnitude-mean()
frequencyBodyGyroscopeMagnitude-std()
frequencyBodyGyroscopeMagnitude-meanFreq()
frequencyBodyGyroscopeJerkMagnitude-mean()
frequencyBodyGyroscopeJerkMagnitude-std()
frequencyBodyGyroscopeJerkMagnitude-meanFreq()
```
The set of variables that were estimated from these signals are: 

```
mean(): Mean value
std(): Standard deviation
```
