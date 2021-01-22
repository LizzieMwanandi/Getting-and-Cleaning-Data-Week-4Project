## 1.MERGING DATA

##1.1 Create a folder in which the data will downloaded

if(!file.exists("WeekFourProject")) { dir.create("WeekFourProject")}

##1.1.1 Downloading the file

DataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(DataUrl, destfile ="./WeekFourProject/UCD_Dataset.zip" ); 

##1.1.2 Unzip files and list files

unzip("UCD_Dataset.zip",exdir = "WeekFourProject")

list.files(path="./WeekFourProject/UCD_Dataset/UCI HAR Dataset")

##1.2read Test data

Subject_test<- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/test/subject_test.txt", header = F)
X_test <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/test/X_test.txt", header = F)
y_test <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/test/y_test.txt", header = F)

## 1.3 read Train data


Subject_train<- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/train/subject_train.txt", header = F)
X_train <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/train/X_train.txt", header = F)
y_train <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/train/y_train.txt", header = F)

##1.4 read Features Data


features <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/features.txt", header = F)
  

##1.5 Read activity labels

activity_labels <- read.table("./WeekFourProject/UCD_Dataset/UCI HAR Dataset/activity_labels.txt", header= F)


##1.6 Create Sanity and Column Values to the Train Data

colnames(X_train) = features[,2]
colnames(y_train) = "activityId"
colnames(Subject_train) = "subjectId"

#1.7 Create Sanity and column values to the test data

colnames(X_test) = features[,2]
colnames(y_test) = "activityId"
colnames(Subject_test) = "subjectId"

#1.8 Create sanity check for the activity labels value

colnames(activity_labels) <- c('activityId','activityType')

##1.9 Merging the train and test data - important outcome of the project

mrg_train = cbind(y_train, Subject_train, X_train)
mrg_test = cbind(y_test, Subject_test, X_test)

#1.9.1 Create the main data table merging both table tables - this is the outcome of 1

setAllInOne = rbind(mrg_train, mrg_test)


# 2. EXTRACTING MEASUREMENTS OF MEAN AND STANDARD DEVIATION 

#2.0   Need step is to read all the values that are available

colNames = colnames(setAllInOne)

#2.1    Need to get a subset of all the mean and standards and the corresponding in activityID and subjectID 

mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))

#2.2  A subset has to be created to get the required dataset

selectedNmaes <- c(as.character(mean_and_std),"activityId","subjectId")
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

##3.  DESCRIPTIVE NAMES FOR THE MEASUREMENTS

setWithActivityNames = merge(setForMeanAndStd, activity_labels, by='activityId', all.x=TRUE)

##4.  SECOND INDEPENDENT TIDY DATA

# 4.0 New tidy set has to be created 

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

#4.1  The last step is to write the output to a text file 
write.table(secTidySet, file = "secTidySet.txt", row.name=FALSE)