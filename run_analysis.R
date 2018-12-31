###Step 1 - Merge Training and Test Data

#Train Data
xtraindata1 <- read.table("./train/X_train.txt", header = FALSE, sep = "", dec = ".")
ytraindata2 <- read.table("./train/y_train.txt")
subtraindata3 <- read.table("./train/subject_train.txt")
train <- rep(1,nrow(xtraindata1))
traindata <- cbind(subtraindata3, train, ytraindata2, xtraindata1)
mycol <- read.table("./features.txt")
mycol <- t(mycol)
names(traindata) <- c("subject", "group", "activity", mycol[2,])

#Test Data
xtestdata1 <- read.table("./test/X_test.txt", header = FALSE, sep = "", dec = ".")
ytestdata2 <- read.table("./test/y_test.txt")
subtestdata3 <- read.table("./test/subject_test.txt")
test <- rep(2,nrow(xtestdata1))
testdata <- cbind(subtestdata3, test, ytestdata2, xtestdata1)
names(testdata) <- c("subject", "group", "activity", mycol[2,])

mydata <- rbind(traindata,testdata)

###Step 2 - Extract Measurements on the Mean and Standard Deviation
meanstd <- (grepl("subject", colnames(mydata)) | grepl("activity", colnames(mydata)) | grepl("mean()", colnames(mydata)) | grepl("std()", colnames(mydata)) )
extrdata <- mydata[, meanstd==TRUE]

###Step 3 - Descriptive Activity Names
actlabel <- read.table("./activity_labels.txt", header = FALSE)
names(actlabel) <- c("activity", "activitylabel")
extrdatawithlabel <- merge(extrdata, actlabel, by="activity", all.x=TRUE)

###step 4 - Label with Descriptive Variable Name
names(extrdatawithlabel) <- gsub("^t", "time", names(extrdatawithlabel))
names(extrdatawithlabel) <- gsub("^f", "frequency", names(extrdatawithlabel))
names(extrdatawithlabel) <- gsub("Acc", "Accelerometer", names(extrdatawithlabel))
names(extrdatawithlabel) <- gsub("Gyro", "Gyroscope", names(extrdatawithlabel))
names(extrdatawithlabel) <- gsub("Mag", "Magnitude", names(extrdatawithlabel))
names(extrdatawithlabel) <- gsub("BodyBody", "Body", names(extrdatawithlabel))

###step 5 - Independent Tidy Data Set
tidydata <- aggregate(. ~subject + activity, extrdatawithlabel, mean)
tidydata <- tidydata[order(tidydata$subject, tidydata$activity),]

write.table(tidydata, "tidydata.txt", row.name=FALSE)
