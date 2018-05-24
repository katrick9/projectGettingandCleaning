library(plyr)
## Downloading dataset
if(!file.exists("./data")){dir.create("./data")}
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "./data/data.zip",method = "curl")

# Unzip dataSet to /data directory
unzip(zipfile = "./data/data.zip",exdir = "./data")

# 1. Merging the training and the test sets to create one data set:
# 1.1 Reading files
# 1.1.1 Reading trainings tables:
Xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
# 1.1.2 Reading testing tables:
Xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")> Ytest <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
# 1.1.3 Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')
# 1.1.4 Reading activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
# 1.2 Assigning column names:
colnames(Xtrain) <- features[,2]
colnames(Ytrain) <- "activityId"
colnames(subjectTrain)<-"subjectId"

colnames(Xtest) <- features[,2]
colnames(Ytest) <- "activityId"
colnames(subjectTest)<-"subjectId"

colnames(activityLabels)<-c('activityId','activityType')
# 1.3 Merging all data in one set:
mrg_train <- cbind(Ytrain, subjectTrain, Xtrain)
mrg_test <- cbind(Ytest, subjectTest, Xtest)
setAllInOne <- rbind(mrg_train, mrg_test)
# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 2.1 Reading column names:
colNames <- colnames(setAllInOne)
mean_and_std <- (grepl("activityId",colnames)|grepl("subjectId" , colNames) | grepl("mean.." , colNames) |  grepl("std.." , colNames) )
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
colNames <- colnames(setAllInOne)
# 2.2 Create vector for defining ID, mean and standard deviation:
mean_and_std <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
)
# 2.3 Making nessesary subset from setAllInOne:
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
# 3. Using descriptive activity names to name the activities in the data set:
setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)
# 4. Appropriately labeling the data set with descriptive variable names.
# This step was made in previos steps =) See 1.3, 2.2, 2.3.
# 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject:
# 5.1 Making second tidy data set 
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

# 5.2 Writing second tidy data set in txt file
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
