run_analysis <- function() {
        ## read in files
        trainData <- read.table("train/X_train.txt")
        trainDataY <- read.table("train/y_train.txt")
        trainSubject <- read.table("train/subject_train.txt")
        testData <- read.table("test/X_test.txt")
        testDataY <- read.table("test/y_test.txt")
        testSubject <- read.table("test/subject_test.txt")
        featuresColNames <- read.table("features.txt")
        activityLabels <- read.table("activity_labels.txt")
        ## create activity names
        testDataYLabel <- ifelse(testDataY == "1", "WALKING", ifelse(testDataY == "2", "WALKING_UPSTAIRS", ifelse(testDataY == "3", "WALKING_DOWNSTAIRS", ifelse(testDataY == "4", "SITTING", ifelse(testDataY == "5", "STANDING", "LAYING")))))
        trainDataYLabel <- ifelse(trainDataY == "1", "WALKING", ifelse(trainDataY == "2", "WALKING_UPSTAIRS", ifelse(trainDataY == "3", "WALKING_DOWNSTAIRS", ifelse(trainDataY == "4", "SITTING", ifelse(trainDataY == "5", "STANDING", "LAYING")))))
        ## coerce factor to character and then label columns
        featuresColNames[, 2] <- as.character(featuresColNames[, 2])
        colnames(trainData) <- c(featuresColNames[ , 2])
        colnames(testData) <- c(featuresColNames[ , 2])
        colnames(trainDataYLabel) <- c("Activity")
        colnames(testDataYLabel) <- c("Activity")
        colnames(trainSubject) <- c("Subject")
        colnames(testSubject) <- c("Subject")
        ## pull out the mean and standard deviation columns as evaluated manually to be desired columns.
        trainDataMeanSD <- trainData[ , c(1:6, 41:46, 81:86, 121:126, 161:166,201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 516:517, 529:530, 542:543, 555:561)]
        testDataMeanSD <- testData[ , c(1:6, 41:46, 81:86, 121:126, 161:166,201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 503:504, 516:517, 529:530, 542:543, 555:561)]
        ## combine with Y column of activities, subject, and columns with mean or SD then combine test and train data sets
        fullTrain <- cbind(trainDataYLabel, trainSubject, trainDataMeanSD)
        fullTest <- cbind(testDataYLabel, testSubject, testDataMeanSD)
        fullData <- rbind(fullTrain, fullTest)
        ## create new table that will average values of each mean and SD column by activity and subject
        averageByActivitySubject <- aggregate(fullData[ ,3:84], by=list(Activity=fullData$Activity, Subject=fullData$Subject), mean)
        write.table(averageByActivitySubject, file = "runAnalysisByActivitySubject.txt")
}