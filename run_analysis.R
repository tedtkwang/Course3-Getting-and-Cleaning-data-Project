


# Merges the training and the test sets to create one data set.

# Combine the 3 separate train data sets
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt",sep="",header=FALSE)   #load measurement data
X_names<-read.table("./UCI HAR Dataset/features.txt",sep="",header=FALSE)   #load measurement data's column labels
names(X_names)<-c("featureID","featureName")   #rename column name
names(X_train)<-X_names$featureName   #rename measurement data column name
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",sep="",header=FALSE)   #load measurement data's activity label
names(y_train)<-"activityID"   #rename column name
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",sep="",header=FALSE)   #load measurement data's subject label
names(subject_train)<-"subjectID"   #rename column name
X_train<-cbind(subject_train,y_train,X_train)   #combine subject, activity, and measurement data


# Do the same steps above to combine the 3 separate test data sets
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt",sep="",header=FALSE)
names(X_test)<-X_names$featureName
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",sep="",header=FALSE)
names(y_test)<-"activityID"
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",sep="",header=FALSE)
names(subject_test)<-"subjectID"
X_test<-cbind(subject_test,y_test,X_test)


X_merged<-rbind(X_train,X_test)   #combine train and test data into one data set


# Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std_features<-grep("-[Mm][Ee][Aa][Nn]|-[Ss][Tt][Dd]",X_names$featureName,value=TRUE)
mean_std_features_index<-grep("-[Mm][Ee][Aa][Nn]|-[Ss][Tt][Dd]",X_names$featureName)
X_merged_mean_std<-X_merged[,c(1,2,mean_std_features_index+2)]   #feature names list did not include subject and activity

# Uses descriptive activity names to name the activities in the data set

activityNames=data.frame(activityID=1:6,activityName=c("walking","walking_upstairs","walking_downstairs","sitting","standing","laying"))
X_merged_mean_std<-merge(X_merged_mean_std,activityNames,by.x="activityID",by.y="activityID",all=FALSE)   #add activityName column
X_merged_mean_std<-X_merged_mean_std[,c(2,82,3:81)]   #re-arrange columns; remove activityID column

# Appropriately labels the data set with descriptive variable names.

### Already completed descriptive variable labeling in steps above

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
X_merged_mean_std_grp<-X_merged_mean_std%>%   #create a second, independent data set
    group_by(subjectID,activityName)%>%   #group by each subject and activity
    summarize_all(mean)   #calculate average of each variable for each group

write.table(X_merged_mean_std,"dataset1.txt",row.name=FALSE)
write.table(X_merged_mean_std_grp,"dataset2.txt",row.name=FALSE)   # submitted tidy data set
