# Step 0: Reading in the test and train data, praparation for Mission 1
# Also, finishing Mission 2. (For definition of "Mission n", see Readme.md;
# for the reason to finish Mission 2 here, see the note ##0.4)
# for attribution to external resources, please see README.md.

## 0.1, read and store Activity and Subject IDs for test.
ytest <- read.table("y_test.txt", col.names=c("ActivityID"))
subjecttest <- read.table("subject_test.txt", col.names=c("SubjectID"))
## 0.2, read the Measurement names. 
measureNames <- read.table("features.txt", col.names=c("MeasureID", "MeasureName"))
## 0.3, read the x_test file with colnames renamed.
Xtest <- read.table("X_test.txt", col.names=measureNames$MeasureName)
## 0.4 Subsetting. I looked through many threads in course forum but still
## I believe that the tidy dataset in Mission 5 is made from all Missions from 1
## through 4; that is, it's a tidy dataset of the extracted one rather than the
## entire one. Otherwise, the extracting stage (Mission 2) will be trivial as the
## extracted dataset will be neither used later or observable by calling the main
## function. (Anyway I really hope the course project description could have been
## clearer about this)
Xtest <- Xtest[,grep(".*mean\\(\\)|.*std\\(\\)", measureNames$MeasureName)]
## 0.5, Put the Act and Subj IDs into the Xtest data
Xtest$ActivityID <- ytest$ActivityID
Xtest$SubjectID <- subjecttest$SubjectID
## Now we have prepared the Xtest data. Same way to prepare the Xtrain.

## 0.6, read and store Activity and Subject IDs for train.
ytrain <- read.table("y_train.txt", col.names=c("ActivityID"))
subjecttrain <- read.table("subject_train.txt", col.names=c("SubjectID"))
## 0.7, read the X_train file with colnames renamed.
Xtrain <- read.table("X_train.txt", col.names=measureNames$MeasureName)
## 0.8 Subsetting.
Xtrain <- Xtrain[,grep(".*mean\\(\\)|.*std\\(\\)", measureNames$MeasureName)]
## 0.9, Put the Act and Subj IDs into the Ytest data
Xtrain$ActivityID <- ytrain$ActivityID
Xtrain$SubjectID <- subjecttrain$SubjectID
## Now we have prepared the test and train sets. End Step 0 and Ready for Step 1.

# Step 1: merging and labeling, cracking Mission 1 and Mission 4

mergeData <- function() {
        data <- rbind(Xtest, Xtrain)##make the new merged dataframe
        cnames <- colnames(data)
        cnames <- gsub("\\.+mean\\.+", cnames, replacement="Means")
        cnames <- gsub("\\.+std\\.+",  cnames, replacement="Stds")
        colnames(data) <- cnames#renamed dataframe
        data#return merged dataframe
}


# Step 2: Adressing Mission 3, Using descriptive activity names to name
# the activities in the data set.
Label <- function(data) {
        activityLabels <- read.table("activity_labels.txt", col.names=c("ActivityID", "ActivityName"))
        activityLabels$ActivityName <- as.factor(activityLabels$ActivityName)
        labeledData <- merge(data, activityLabels)#Labeling data
        labeledData# return the labeled data
}

# Step 3 Mission 5, making the tidy independant data.
main <- function(name) {
        library(reshape2)
        # As taught in lecture, melting the dataset
        measureVars = setdiff(colnames(Label(mergeData())), c("ActivityID", "ActivityName", "SubjectID"))
        meltedData <- melt(Label(mergeData()), id=c("ActivityID", "ActivityName", "SubjectID"), measure.vars=measureVars)    
        # recast to generate the tidydata
        tidyData <- dcast(meltedData, ActivityName + SubjectID ~ variable, mean)    
        write.table(tidyData, name)

}

# Step 4: Calling the main function

main("tidyData.txt")
