############ GACD project 1

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

## Make file for data
setwd("C:/Users/ragna/Dropbox/Doktor/Afangar/Dataspecialization/3GettingAndCleaningData")

if (!file.exists("P1")) {
        dir.create("P1")
}


######## 1. Merge the training and the test sets to create one data set

# Read in the test, train, and subject data
xtrain <- read.table("./P1/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./P1/UCI HAR Dataset/train/Y_train.txt")
subjecttrain <- read.table("./P1/UCI HAR Dataset/train/subject_train.txt")

xtest <- read.table("./P1/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./P1/UCI HAR Dataset/test/Y_test.txt")
subjecttest <- read.table("./P1/UCI HAR Dataset/test/subject_test.txt")

# Read in the column headings from features.txt  
features <- read.table("./P1/UCI HAR Dataset/features.txt", 
                       stringsAsFactors = F)
colnames(features) <- c("featureID", "feature")


# Put in column headings in datasets
colnames(xtrain) <- features$feature
colnames(xtest) <- features$feature

# Assign user friendly headings to Training and Test labels
colnames(ytrain) <- c("actID")
colnames(ytest) <- c("actID")

# Assign user friendly headings to subjects
colnames(subjecttrain) <- c("subID")
colnames(subjecttest) <- c("subID")

# Read activity labels
actlabels <- read.table("./P1/UCI HAR Dataset/activity_labels.txt", 
                     stringsAsFactors = T)

# Assign user friendly headings to actlabels
colnames(actlabels) <- c("actID", "activity")

# Combine train and test data into one dataset
xcomb <- rbind(xtrain,xtest)
ycomb <- rbind(ytrain,ytest)
subcomb <- rbind(subjecttrain,subjecttest)

# Combine xcomb, ycomb, and subcomb in one dataset
fullData <- cbind(xcomb, ycomb, subcomb)

###############################################################################
############# 2. Extract only the measurements on the mean and standard 
# deviation for each measurement. 

used = "mean|std|subID|actID|activity"
tidyData = fullData[,grep(used, names(fullData), value=TRUE)]

###############################################################################
######### 3. Uses descriptive activity names to name the activities in the data 
######### set

# Put activity names in dataset according to activity ID
tidyData$activity <- as.character((factor(tidyData$actID, 
                                          labels = actlabels$activity))) 

###############################################################################
########## 4. Appropriately label the data set with descriptive variable names.

# Take "()" out of headings
colnames(tidyData) <- sapply(colnames(tidyData), function(x) 
        gsub("[()]", "", x, ignore.case = TRUE))

# Take "-" out of headings
colnames(tidyData) <- sapply(colnames(tidyData), function(x) 
        gsub("[-]", "", x, ignore.case = TRUE))


###############################################################################
############## 5. From the data set in step 4, create a second, independent 
########## tidy data set with the average of each variable for each activity 
######## and each subject.

# Change activity ID to a factor variable
tidyData$actID <- as.factor(tidyData$actID)

# Create dataset with the average of each variable for each activity for 
# each subject
subtidyData <- aggregate(. ~ activity + subID, data = tidyData, FUN = mean)

# Save sub dataframe
write.table(subtidyData, file= "./P1/SamsungGalaxySubset.txt", row.names=F)
