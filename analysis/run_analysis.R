##After moving the following files into one folder, we can do analysis by calling functions below:
test <- read.table("X_test.txt")
train <- read.table("X_train.txt")
dataset <- rbind(train,test)
features <- read.table("features.txt")[,2]
features <- features[,2]
colnames(dataset) <- features
features <- as.character(features)
feature_mean <- grep("mean()",features)#, fixed=TRUE)
feature_std <- grep("std()",features)#, fixed=TRUE)
features <- c(feature_mean,feature_std)
features <- sort(features)
dataset_filter <- dataset[,features]
##subject and class
s_test <- read.table("subject_test.txt")
s_train <- read.table("subject_train.txt")
s_dataset <- rbind(s_train,s_test)
colnames(s_dataset) <- c("subject")
y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")
y_dataset <- rbind(y_train,y_test)
##activities
activities <- read.table("activity_labels.txt")
##merge
activity <- (merge(activities, y_dataset, by = 'V1',sort=FALSE))[,2]
##combine
dataset <- cbind(s_dataset,activity,dataset_filter)
##average
group <- split(dataset, list(dataset$subject,dataset$activity))
result <- do.call(rbind, lapply(group, function(x) colMeans(x[,c(3:68)])))
subject_activity <- rownames(result)
list_subject_activity <- strsplit(subject_activity, "\\.")
matrix_list_subject_activity <- matrix(unlist(list_subject_activity), ncol = 2, byrow = TRUE)
colnames(matrix_list_subject_activity) <- c("subject","activity")
result <- cbind(matrix_list_subject_activity,result)
result <- result[order(result[,1],result[,2]),]
write.csv(x=result,"result.csv")
