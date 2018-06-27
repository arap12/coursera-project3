## read the file from URL
filename <- "dataset.zip"
fURL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fURL,filename)
#Load all the datasets from file
xtrain <- read.table(unz(filename,"UCI HAR Dataset/train/X_train.txt"))
ytrain <- read.table(unz(filename,"UCI HAR Dataset/train/y_train.txt"))
xtest <- read.table(unz(filename,"UCI HAR Dataset/test/X_test.txt"))
ytest <- read.table(unz(filename,"UCI HAR Dataset/test/y_test.txt"))
strain <- read.table(unz(filename,"UCI HAR Dataset/train/subject_train.txt"))
stest <- read.table(unz(filename,"UCI HAR Dataset/test/subject_test.txt"))
activity <- read.table(unz(filename,"UCI HAR Dataset/activity_labels.txt"))
names <- read.table("features.txt")
colnames(activity) <- c("id","Activity")
train <- cbind(xtrain,ytrain)
test <- cbind(xtest,ytest)
#Merge Train and Test sets
subject <-rbind(strain,stest)
myData <- rbind(train,test)
#add descriptive label names for variables
colnames(myData) <- names$V2
colnames(myData)[length(colnames(myData))] <- c("Activity")
myData <- cbind("sID"=subject$V1,myData)
#replace activity IDs with  descriptive activity names for Data set
myData[["Activity"]] <- activity[ match(myData[["Activity"]],activity[["id"]] ) , "Activity"]
#Extract mean and std columns
meanids <- grep('mean',colnames(myData))
meancols <- myData[,c(meanids)]
stdids <- grep('std',colnames(myData))
stdcols <- myData[,c(stdids)]
#Create an independent dataset with average for each activity for each subject
len <- length(unique(myData$sID))*length(unique(myData$Activity))
averageData <- data.frame(row.names=1:len )
for (i in (2:562))
{
x <- data.frame(aggregate(x=myData[,i],
                             by=list(myData$sID,myData$Activity),
                             FUN=mean))

averageData <-cbind(averageData,x[,3])
colnames(averageData)[i-1] <- colnames(myData)[i]
}
colnames(x)[1:2] <- c("Subject","Activity")
averageData<-cbind("Subject"=x$Subject,"Activity"=x$Activity,averageData)
write.table(averageData, "average.txt", row.names = FALSE, quote = FALSE)
