#Before processing data, we have to move the following files into analysis folder:
##  X_train.txt, X_test.txt
##  subject_train.txt, subject_test.txt
##  y_train.txt, y_test.txt
##  features.txt, activity_labels.txt

#1-Merges the training and the test sets to create one data set.
##Read data from train and test files
train <- read.table("./analysis/X_train.txt")
test <- read.table("./analysis/X_test.txt")

##Merge train data and test data
dataset <- rbind(train,test)

##Add features
features <- read.table("./analysis/features.txt")[,2]
features <- as.character(features)
colnames(dataset) <- features


#2-Extracts only the measurements on the mean and standard deviation for each measurement
##Filter feature
feature_mean <- grep("mean()",features, fixed=TRUE)
feature_std <- grep("std()",features, fixed=TRUE)
features <- sort(c(feature_mean,feature_std))
##Filter dataset
dataset_filter <- dataset[,features]


#3-Uses descriptive activity names to name the activities in the data set
#4-Appropriately labels the data set with descriptive activity names.

##Load subjects from files
s_train <- read.table("./analysis/subject_train.txt")
s_test <- read.table("./analysis/subject_test.txt")
s_dataset <- rbind(s_train,s_test)
colnames(s_dataset) <- c("subject")

##Load activity codes from files
y_train <- read.table("./analysis/y_train.txt")
y_test <- read.table("./analysis/y_test.txt")
y_dataset <- rbind(y_train,y_test)

##Load activity labels from file
activities <- read.table("./analysis/activity_labels.txt")

##Match activity names and activity codes
activity <- (merge(activities, y_dataset, by = 'V1',sort=FALSE))[,2]

##combine subjects, activities and features into one dataset
dataset <- cbind(s_dataset,activity,dataset_filter)

#5-Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##divide dataset into groups by activity and subject
group <- split(dataset, list(dataset$subject,dataset$activity))
number_of_feature <- ncol(dataset)

##calculate avarage value of each variable
result <- do.call(rbind, lapply(group, function(x) colMeans(x[,c(3:number_of_feature)])))

##get subjects and activities
subject_activity <- rownames(result)
list_subject_activity <- strsplit(subject_activity, "\\.")
matrix_subject_activity <- matrix(unlist(list_subject_activity), ncol = 2, byrow = TRUE)
colnames(matrix_subject_activity) <- c("subject","activity")

##combine subjects, activities and average values
result <- cbind(matrix_subject_activity,result)

##order dataset by 
result <- result[order(as.numeric(result[,1]),result[,2]),]
write.csv(x=result,"tiny_data.csv",quote=FALSE,row.names=FALSE)
