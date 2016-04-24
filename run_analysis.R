assignIDs <- function(theData){
  theData$ID <- as.numeric(rownames(theData))
  return(theData)
}
## LOAD DATA
trainingSubject = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("subjectID"))
trainingLabels = read.table("UCI HAR Dataset/train/y_train.txt", col.names = c("labelID"))
trainingSet = read.table("UCI HAR Dataset/train/X_train.txt")
testSubject = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subjectID"))
testLabels = read.table("UCI HAR Dataset/test/y_test.txt", col.names = c("labelID"))
testSet = read.table("UCI HAR Dataset/test/X_test.txt")

## ASSIGN ID'S
trainingSubject <- assignIDs(trainingSubject)
trainingLabels <- assignIDs(trainingLabels)
trainingSet <- assignIDs(trainingSet)
testSubject <- assignIDs(testSubject)
testLabels <- assignIDs(testLabels)
testSet <- assignIDs(testSet)
## CREATE TRAINING SET
training <- merge(trainingSubject, trainingLabels, all = TRUE)
training <- merge(training, trainingSet, all = TRUE)

## CREATE TEST SET
test <- merge(testSubject, testLabels, all = TRUE)
test <- merge(test, testSet, all = TRUE)

## CREATE FINAL SET
finalSet <- rbind(test, training)


## Extracts only the measurements on the mean and standard deviation for each measurement.

features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
output <-finalSet[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

## Uses descriptive activity names to name the activities in the data set.

activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),)
output2 = merge(output, activity_labels)

## Appropriately labels the data set with descriptive activity names.
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
  colnames(output2)[i + 3] <- selected_features$feature_label[i]
}
finalOutput = output2

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

drops <- c("ID","activity_label")
finalOutput2 <- finalOutput[,!(names(finalOutput) %in% drops)]
aggregatedData <- aggregate(finalOutput2, by=list(subject = finalOutput2$subjectID, activity = finalOutput2$activity_id), FUN=mean, na.rm = TRUE)
drops <- c("subject","activity")
aggregatedData <- aggregatedData[,!(names(aggregatedData) %in% drops)]
aggregatedData = merge(aggregatedData, activity_labels)
write.table(aggregatedData, file = "cleaned.txt", row.names = FALSE)