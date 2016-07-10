###### Purpose
# Instructions
# 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# Review criterialess 
# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# The README that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.
# Getting and Cleaning Data Course Projectless 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck!
# 
######
###### Author: Keith Bailey
###### Date: 2016-07-09



#Download dataset
if(!file.exists("./data")){dir.create("./data")}
fileURL1<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL1, destfile="./data/dataset.zip",method="curl")
unzip("./data/dataset.zip", exdir="./data/dataset")

#######################################################################################################
###### 1. Merges the training and the test sets to create one data set.                         #######
#######################################################################################################


#create a vector to hold the names of the two X_ files we want to import;
X_<-c("./data/dataset/UCI HAR Dataset/test/X_test.txt","./data/dataset/UCI HAR Dataset/train/X_train.txt")
bindData_tempX1<-read.table(file=X_[1])
bindData_tempX2<-read.table(file=X_[2])
bindDataX<-rbind(bindData_tempX1, bindData_tempX2)

#rename the columns of the consolidated dataset to those specified in the features file
#also rename the training labels column we added to our dataset to training labels
features<-read.table("./data/dataset/UCI HAR Dataset/features.txt")
names(bindDataX)<-c(as.vector(features$V2))

#######################################################################################################
###### 2. Extracts only the measurements on the mean and standard deviation for each measurement#######
#######################################################################################################


#We will need to establish which variables names have mean and standard deviation (std) in them
#In order to do this we will have to use grepl and regular expressions to search for matches
bindDataX.MeanStd <- bindDataX[,grepl("mean\\(\\)|std\\(\\)",as.vector(features$V2))]


#######################################################################################################
###### 3. Uses descriptive activity names to name the activities in the data set                   #######
#######################################################################################################

Y_<-c("./data/dataset/UCI HAR Dataset/test/y_test.txt","./data/dataset/UCI HAR Dataset/train/y_train.txt")
bindData_tempY1<-read.table(file=Y_[1])
bindData_tempY2<-read.table(file=Y_[2])
bindDataY<-rbind(bindData_tempY1, bindData_tempY2)

#rename the columns of bindData Y
names(bindDataY)<-c("activityLabelNo")

#Not explicitly stated in the project requirements yet, but logical to bring in the subjects before producing the final dataset

Subjects<-c("./data/dataset/UCI HAR Dataset/test/subject_test.txt","./data/dataset/UCI HAR Dataset/train/subject_train.txt")                 
bindData_tempSubjects1<-read.table(file=Subjects[1])
bindData_tempSubjects2<-read.table(file=Subjects[2])
bindDataSubjects<-rbind(bindData_tempSubjects1, bindData_tempSubjects2)
#rename the columns of bindData Subjects
names(bindDataSubjects)<-c("subjects")

#add the activitylabel numbers and subjects to the dataset
bindDataX.MeanStd.Y<-cbind(bindDataX.MeanStd,bindDataY,bindDataSubjects)


#Next we need to transform our activityLabels into meaningful names 
#as described in the activity_labels file

#read in the activity labels
activityLabels<-read.table(file="./data/dataset/UCI HAR Dataset/activity_labels.txt")
#rename activity label columns
names(activityLabels)<-c("activityLabelNo","activityLabelName")

#It is not absolutely necessary to name both the x and y table columns names twice,
#but I do so as it is a good habit to get into as often the names will not be the same 
finalDataSet<-merge(bindDataX.MeanStd.Y, activityLabels, by=c("activityLabelNo","activityLabelNo"))


#######################################################################################################
###### 4. Appropriately labels the data set with descriptive variable names.                    #######
#######################################################################################################

#Rename the final dataset to make the variables more meaningful
#Note that I have resisted temptation to rename Acc to Acceration, Mag to Magnitude etc as this is clearly
#laid out in the codebook. I believe that the names already convey meaning and a thorough understanding of
#this dataset can only truely be obtained from reading the accompanying codebook
names(finalDataSet)<-gsub('[-()]', '', names(finalDataSet))

#change to factors to remove ambiguity
finalDataSet$subjects <- as.factor(finalDataSet$subjects)

#######################################################################################################
###### 5. From the data set in step 4, creates a second, independent tidy data set with the      ######
######    average of each variable for each activity and each subject.                           ######
#######################################################################################################

require(tidyr)
require(dplyr)

#I have interpretted the tidy dataset requirement to be a tall narrow dataset with one observation per
#row as all data herein are sensor signals. I understand that might not be the way you, my reviewer have
#implemented this, arguing instead that each row are one observation each of which has many signals.

#Tidy dataset narrow
finalDataset.tidynarrow <- finalDataSet %>%gather(measurementType, measurement, -subjects, -activityLabelNo, -activityLabelName)


#Create a summarised dataset showing the average for each activity and subject.
#In this case there are no NA data, but again leaving for good practice
finalDataset.tidynarrow.averages <- finalDataset.tidynarrow %>% group_by(subjects, activityLabelName, measurementType) %>% summarise(average = mean(measurement, na.rm=TRUE))

write.table(finalDataset.tidynarrow.averages, file="tidyDataset.txt", sep=" ", row.names=FALSE)
