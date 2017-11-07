# Getting-and-Cleaning-Data
Assignment: Getting and Cleaning Data Course Project

The goal of this exercise is to upload raw data, consolidate and clean it an to produce a tidy data set with the relevant data. The data is available from: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

The steps are the following:

1) Download the dataset, unzip and save on the local PC.
2) Merge the training and test sets of data.
3) Extracts only the measurements on the mean and standard deviation for each measurement.
4) Uses the available descriptive activity names to name the activities in the data set itself.
5) Labels the data set with descriptive variable names.
6) Deliver the tidy data set with the average of each variable for each activity and each subject.
