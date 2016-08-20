## This seems like the sort of program that should start with a function.
## There are no function variables because it doesn't need function variables.

run_analysis <- function (){
  
## This checks for, downloads, saves and unpacks the data set.
  
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/gettingandcleaningdataset.zip")  
  unzip(zipfile="./data/gettingandcleaningdataset.zip",exdir="./data")

## This reads in and saves the needed columns.
  
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  features <- read.table('./data/UCI HAR Dataset/features.txt')
  activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')
  
## This part renames the needed columns.
  
  colnames(x_train) <- features[,2] 
  colnames(y_train) <-"activityId"
  colnames(subject_train) <- "subjectId"
  colnames(x_test) <- features[,2] 
  colnames(y_test) <- "activityId"
  colnames(subject_test) <- "subjectId"
  colnames(activityLabels) <- c('activityId','activityType')

## This merges all needed columns. (Part 1)
  
  trainmerge <- cbind(y_train, subject_train, x_train)
  testmerge <- cbind(y_test, subject_test, x_test)
  fullmergeset <- rbind(trainmerge,testmerge)  
  fullcolnames <- colnames(fullmergeset)
  
## This cleans the full set of data to return only means and standard deviations. (Part 2)
  
  meanandstd <- (grepl("activityId" , fullcolnames) | 
                 grepl("subjectId" , fullcolnames) | 
                 grepl("mean.." , fullcolnames) | 
                 grepl("std.." , fullcolnames)
                 )
  setmenadandstd <- fullmergeset[ , meanandstd == T]
  
## This adds descriptive activity names to the data set. (Part 3 & 4)
  
  setactivitynames <- merge(setmeanandstd, activityLabels, by='activityId',all.x=T)
  
## This creates the tidy independent data set and writes it into a text file. (Part 5)
  
  tidyset <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
  tidyset <- tidyset[order(tidyset$subjectId, tidyset$activityId),]
  write.table(tidyset, "Tidy Data Set.txt", row.name=FALSE)
}