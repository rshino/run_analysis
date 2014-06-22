# Coursera 
# Getting and Cleaning Data
# by Jeff Leek, PhD, Roger D. Peng, PhD, Brian Caffo, PhD
# Peer Assignment
# 06/22/2014
#
# Directions
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity 
#    and each subject. 
#
# setwd("/nfs-thecus/home/shino/R-programs/03. Getting and Cleaning Data/Project")

run_analysis <- function(){

dataDir<-"./UCI HAR Dataset"
if(!file.exists(dataDir)){
    dataDir<-"."
}
trainx_file<-"train/X_train.txt"
trainy_file<-"train/y_train.txt"
trainsub_file<-"train/subject_train.txt"
testx_file<-"test/X_test.txt"
testy_file<-"test/y_test.txt"
testsub_file<-"test/subject_test.txt"

print("Reading Files")
# read in the features ontaining all of the separate types of observations
features<-read.table(paste(dataDir,"features.txt",sep="/"));

# read in 3 data files for training data
trainx<-read.table(paste(dataDir,trainx_file,sep="/")); colnames(trainx)<-features[,2];
trainy<-read.table(paste(dataDir,trainy_file,sep="/"));
trainsub<-read.table(paste(dataDir,trainsub_file,sep="/")); 

# read in 3 data files for test data
testx<-read.table(paste(dataDir,testx_file,sep="/")); colnames(testx)<-features[,2];
testy<-read.table(paste(dataDir,testy_file,sep="/"));
testsub<-read.table(paste(dataDir,testsub_file,sep="/")); 

# convert index into integers
features$V1<-as.numeric(features$V1)
dataColumns<-nrow(features)

# set row numbers (rn) as column for training files, used to join_all them into a single dataframe
trainx$rn<-as.numeric(rownames(trainx))
trainy$rn<-as.numeric(rownames(trainy))
trainsub$rn<-as.numeric(rownames(trainsub))
colnames(trainsub)[1]<-"Subject"  # useful column name
colnames(trainy)[1]<-"Activity"   # useful column name

# set row numbers (rn) as column for test files, used to join_all them into a single dataframe
testx$rn<-as.numeric(rownames(testx))
testy$rn<-as.numeric(rownames(testy))
testsub$rn<-as.numeric(rownames(testsub))
colnames(testsub)[1]<-"Subject"  # useful column name
colnames(testy)[1]<-"Activity"   # useful column name


library(plyr)

print("Merging data")
dfList<-list(trainsub, trainy, trainx)  # list of data frames to combine horizontally
train<-join_all(dfList,by="rn")         # merge the data frames horizontally using join_all
train$rn<-NULL                          # get rid of the index, we don't need it anymore

dfList<-list(testsub, testy, testx)     # list of data frames to combine horizontally
test<-join_all(dfList,by="rn")          # merge the data frames horizontally using join_all
test$rn<-NULL                           # get rid of the index, we don't need it anymore

# combine test and training results "vertically"
rawresults<-rbind(test,train)

# construct clean results with only the data we need
cleanresults<-data.frame(rawresults[,1:2])

print("Creating clean results")
# copy over only mean and std data from raw into clean results
for(i in 1:ncol(rawresults))
{   
    cn<-colnames(rawresults)[i];   
    if(length(grep("(-mean()|-std())",cn,ignore.case = TRUE))!=0)  # seek column names containing mean and std
    {
        cleanresults[cn]=rawresults[cn]
 #       lapply(cleanresults[cn],as.numeric)
    }
}

# lapply(cleanresults,as.numeric)
# Observation: note backquote to protects against special characters in the name
# ddply(cleanresults, .(Subject, Activity), summarise,avg=mean(`tBodyAcc-mean()-X`))

# clean up the column names a bit to "legal" characters, will help humans looking at this data
colnames(cleanresults)<-gsub("[-]","_",colnames(cleanresults))
colnames(cleanresults)<-gsub("[()]","",colnames(cleanresults))

print("Summarizing results")
cleansummary<-ddply(cleanresults, .(Subject, Activity),  numcolwise(mean))
# test for average of Subject 1, Activity 1, it's equal to [1] 0.2773308
# mean(cleanresults[cleanresults$Subject==1 & cleanresults$Activity ==1,3])
print("Writing results")
write.table(cleansummary,"cleansummary.csv",sep = ",", row.names=FALSE)
print("Complete.")
}
