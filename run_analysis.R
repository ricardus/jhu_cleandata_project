###### This is the main script of the project to run the analysis on data

### Step 0/ First we check if the original data of the study is in place. If TRUE, then we create the new output dir
mainDir <- getwd()
rawData <- paste(mainDir, sep = "/", "Data")
outputData <- paste(mainDir, sep = "/", "Ouput")


if (!file.exists(rawData)){
    stop("The original data to perform the analysis is not present")
} else {
    dir.create(outputData, showWarnings = FALSE)
}


### Step 1/ Merge the training and the test sets to create one data set

# Getting the data from "train" dataset first

trainSet <- paste(rawData, sep = "/", "train")
subjectTrain <- paste(trainSet, sep = "/", "subject_train.txt")
xTrain <- paste(trainSet, sep = "/", "X_train.txt")
yTrain <- paste(trainSet, sep = "/", "y_train.txt")

subjectData_train <- read.table(subjectTrain, header = TRUE)
xData_train <- read.table(xTrain, header = TRUE)
yData_train <- read.table(yTrain, header = TRUE)

# Getting the data from "test" dataset first

testSet <- paste(rawData, sep = "/", "test")
subjectTest <- paste(testSet, sep = "/", "subject_test.txt")
xTest <- paste(testSet, sep = "/", "X_test.txt")
yTest <- paste(testSet, sep = "/", "y_test.txt")

subjectData_test <- read.table(subjectTest, header = TRUE)
xData_test <- read.table(xTest, header = TRUE)
yData_test <- read.table(yTest, header = TRUE)

# Merge by binding columns all objects from datasets into two: train and test

trainData <- cbind(subjectData_train, yData_train, xData_train)
testData <- cbind(subjectData_test, yData_test, xData_test)

# Merge by binding rows those two into one: mergedData (needs some colnames magic to get them identical)

names(testData) <- names(trainData)
mergedData <- rbind(testData, trainData)


### Step 2/ Extract only the measurements on the mean and standard deviation for each measurement

# First, we need to setup the proper column names for the dataset (found on features.txt)

featuresDir <- paste(rawData, sep = "/", "features.txt")
restcols <- read.table(featuresDir, header = FALSE)

colnames(mergedData) <- c("subject", "activity", as.character(restcols[,2]))

# Then, we need to find which of these columns meet the requirements. Since there are two ways of approach it (either 
# select "mean" or "mean()"), I will go for the second one.

library(data.table)
mergedData <- cbind(mergedData[,1:2], mergedData[,colnames(mergedData) %like% "mean()" | colnames(mergedData) %like% "std()"])


### Step 3/ Uses descriptive activity names to name the activities in the data set

activitiesDir <- paste(rawData, sep = "/", "activity_labels.txt")
actlabels <- read.table(activitiesDir, header = FALSE)

# We now replace every activity number on mergedData with its activity label from activity_labels.txt

for(i in 1:nrow(actlabels)){
    mergedData$activity <- gsub(actlabels[i,1],actlabels[i,2], mergedData$activity)
}


### Step 4/ Appropriately label the data set with descriptive variable names

# Changes: no "-" and "()", freq and time names and all in lowercase

colnames(mergedData) <- sub("-","", colnames(mergedData))
colnames(mergedData) <- sub("()","", colnames(mergedData))
colnames(mergedData) <- sub("^t","time", colnames(mergedData))
colnames(mergedData) <- sub("^f","freq", colnames(mergedData))
colnames(mergedData) <- tolower(colnames(mergedData))


### Step 5/ From the dataset in step 4, create a second, independent tidy data set with the average of each 
### variable for each activity and each subejct

# First we group our data by subject and then by activity. We then get the average for each column of the dataset

outputData <- mergedData %>% group_by(subject,activity) %>% summarise_each(funs(mean))



### End of Script :)
