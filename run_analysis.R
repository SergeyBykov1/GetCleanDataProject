#
# Getting and Cleaning Data. Course project.
#

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set  
# with the average of each variable for each activity and each subject.

require(plyr)

pathWork <- getwd()
pathData <- file.path(pathWork, "UCI HAR Dataset")
# list.files(pathData, recursive=TRUE)

####################
# 0. Read raw data
####################

dataFeatures <- read.table(file.path(pathData, "features.txt"), 
                           colClasses = c("character"))
dataLabels <- read.table(file.path(pathData, "activity_labels.txt"))

dataSubjTrain <- read.table(file.path(pathData, "train", "subject_train.txt"))
dataSubjTest <- read.table(file.path(pathData, "test" , "subject_test.txt" ))

dataYTrain <- read.table(file.path(pathData, "train", "Y_train.txt"))
dataYTest <- read.table(file.path(pathData, "test" , "Y_test.txt" ))

dataXTrain <- read.table(file.path(pathData, "train", "X_train.txt"))
dataXTest <- read.table(file.path(pathData, "test" , "X_test.txt"))

####################
# 1. Merges the training and the test sets to create one data set.
####################

# Bind sensor data
dataTrain <- cbind(cbind(dataXTrain, dataSubjTrain), dataYTrain)
dataTest <- cbind(cbind(dataXTest, dataSubjTest), dataYTest)
dataMerged <- rbind(dataTrain, dataTest)

# Label columns
names(dataMerged) <- 
    rbind(rbind(dataFeatures, c(562, "Subject")), c(563, "ActivityId"))[,2]

####################
# 2. Extract only the mean and standard deviation
####################

dataMeanStd <- dataMerged[,grepl("mean|std|Subject|ActivityId", 
                                 names(dataMerged))]

####################
# 3. Use descriptive activity names
####################

names(dataLabels) = c("ActivityId", "Activity")
dataMeanStd <- join(dataMeanStd, dataLabels, 
                    by = "ActivityId", match = "first")
dataMeanStd <- dataMeanStd[,-1]

####################
# 4. Appropriately labels the data set
####################

# Remove parentheses and make names
names(dataMeanStd) <- gsub('\\(|\\)',"",names(dataMeanStd), perl = TRUE)
names(dataMeanStd) <- make.names(names(dataMeanStd))
# Create understandable names
names(dataMeanStd) <- gsub('Acc',"Acceleration", names(dataMeanStd))
names(dataMeanStd) <- gsub('GyroJerk',"AngularAcceleration", names(dataMeanStd))
names(dataMeanStd) <- gsub('Gyro',"AngularSpeed",names(dataMeanStd))
names(dataMeanStd) <- gsub('Mag',"Magnitude",names(dataMeanStd))
names(dataMeanStd) <- gsub('^t',"TimeDomain.",names(dataMeanStd))
names(dataMeanStd) <- gsub('^f',"FrequencyDomain.",names(dataMeanStd))
names(dataMeanStd) <- gsub('\\.mean',".Mean",names(dataMeanStd))
names(dataMeanStd) <- gsub('\\.std',".StandardDeviation",names(dataMeanStd))
names(dataMeanStd) <- gsub('Freq\\.',"Frequency.",names(dataMeanStd))
names(dataMeanStd) <- gsub('Freq$',"Frequency",names(dataMeanStd))

####################
# 5. Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject
####################

tidy_result = ddply(dataMeanStd, c("Subject","Activity"), numcolwise(mean))
write.table(tidy_result, file = "./tidy_data.txt", row.names=FALSE)

