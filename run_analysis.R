# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

makeTidyDataSet <- function() {  
        downloadAndExtractFiles()
        mergedData <- mergeSets()
        generateTidyData(mergedData)
}


downloadAndExtractFiles <- function() {
        if(!file.exists("data")){
                dir.create("data")
        }
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        
        
        file <- "data/samsungData.zip"
        if(!file.exists(file)){
                download.file(url = fileURL,destfile = file ,method = "curl")
        }

        unzip(file)
}

mergeSets <- function(){
        # Features
        features <- scan("UCI HAR Dataset/features.txt", what=list(integer(), character()))
        feature_labels <- features[[2]]
        
        # Activities
        activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, 
                                    col.names=c("ACTIVITY_ID", "ACTIVITY"))
        
        # Load Data
        if (!file.exists("test.raw.RData")) {
                # Test Data
                subjects_test = scan("UCI HAR Dataset/test/subject_test.txt", what=integer())
                x_test = read.table("UCI HAR Dataset/test/X_test.txt", col.names=feature_labels, check.names = FALSE)
                y_test = scan("UCI HAR Dataset/test/y_test.txt",what=integer())
                y_test.activity = activity_labels[,"ACTIVITY"]
                
                test_data <- data.frame(subject=subjects_test,activity=y_test.activity[y_test],x_test,check.names=FALSE)
                
                save(test_data, file="test.raw.RData")
        } else {
                load("test.raw.RData")
        }
        
        if (!file.exists("train.raw.RData")) {
                # Train Data
                subjects_train = scan("UCI HAR Dataset/train/subject_train.txt", what=integer())
                x_train = read.table("UCI HAR Dataset/train/X_train.txt", col.names=feature_labels, check.names=FALSE)
                y_train = scan("UCI HAR Dataset/train/y_train.txt",what=integer())
                y_train.activity = activity_labels[,"ACTIVITY"]
                
                train_data <- data.frame(subject=subjects_train,activity=y_train.activity[y_train],x_train,check.names=FALSE)
                
                save(train_data, file="train.raw.RData")
        } else {
                load("train.raw.RData")
                
        }
        
        # Merge both Data Sets
        mergedData <- rbind(train_data, test_data)
        mDStd <- grep("std\\(\\)", colnames(mergedData))
        mDMean <- grep("mean\\(\\)", colnames(mergedData))
        joinLabels <- sort(union(mDStd, mDMean))
        
        mergedData[,c(1,2,joinLabels)]
}

generateTidyData <- function(mergedData) {
        
        subjectAndActivitiesMeans <- t(sapply(split(mergedData, list(mergedData$subject, mergedData$activity)), function(z) apply(z[,-c(1,2)],2,mean)))
        subjectActivities <- do.call("rbind", strsplit(rownames(subjectAndActivitiesMeans), split="\\."))
        out <- data.frame( SUBJECT=as.numeric(subjectActivities[,1]), 
                              ACTIVITY=subjectActivities[,2],
                              subjectAndActivitiesMeans,
                              check.names=FALSE,
                              row.names=NULL)
        # Save tidy data
        write.table(out, file="outTidyData.txt", quote=FALSE, row.names=FALSE)
}