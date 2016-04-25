## Read in the features.txt file as a vector to use as the column names
library(dplyr); library(stringr)
features <- read.delim("features.txt", header = FALSE, sep = " ")
columnNames <- as.vector(features[[2]])

## Read in the test and train data sets
xtest <- read.fwf("X_test.txt", rep.int(16,561), col.names = columnNames)
xtrain <- read.fwf("X_train.txt", rep.int(16,561), col.names = columnNames)

## Read in the test and train Activity ID (y) and the Subject ID files
## Also the activity names
ytest <- read.delim("y_test.txt", header = FALSE, col.names = "activityid")
ytrain <- read.delim("y_train.txt", header = FALSE, col.names = "activityid")
subjecttest <- read.delim("subject_test.txt", header = FALSE, col.names = "subjectid")
subjecttrain <- read.delim("subject_train.txt", header = FALSE, col.names = "subjectid")
activities <- read.delim("activity_labels.txt", header = FALSE, sep = " ", 
                         col.names = c("activityid","activitylabel"))

## Extract only the meand and standard deviation columns
testdata <- tbl_df(xtest[,grep("[Mm]ean[!Ff]|std",names(xtest), value = TRUE)])
traindata <- tbl_df(xtrain[,grep("[Mm]ean[!Ff]|std",names(xtest), value = TRUE)])

## Add the Activity and Subject ID to the xtrain and xtest main data frames
testdata$activityid <- ytest$activityid
traindata$activityid <- ytrain$activityid
testdata$subjectid <- subjecttest$subjectid
traindata$subjectid <- subjecttrain$subjectid

## Group by the activityid and subjectid before merging to avoid duplication
testdata1 <- testdata %>% group_by(activityid,subjectid) %>% 
     summarize(testtbodyaccmeanx = mean(tBodyAcc.mean...X), 
               testtbodyaccmeany = mean(tBodyAcc.mean...Y), 
               testtbodyaccneanz = mean(tBodyAcc.mean...Z),
               testtbodyaccstdx = mean(tBodyAcc.std...X), 
               testtbodyaccstdy = mean(tBodyAcc.std...Y), 
               testtbodyaccstdz = mean(tBodyAcc.std...Z), 
               testtgravityaccmeanx = mean(tGravityAcc.mean...X), 
               testtgravityaccmeany = mean(tGravityAcc.mean...Y), 
               testtgravityaccmeanz = mean(tGravityAcc.mean...Z), 
               testtgravitystdx = mean(tGravityAcc.std...X), 
               testtgravityaccstdy = mean(tGravityAcc.std...Y), 
               testtgravityaccstdz = mean(tGravityAcc.std...Z),
               testtbodyaccjerkmeanx = mean(tBodyAccJerk.mean...X), 
               testtbodyaccjerkmeany = mean(tBodyAccJerk.mean...Y), 
               testtbodyaccjerkmeanz = mean(tBodyAccJerk.mean...Z),
               testtbodyaccjerkstdx = mean(tBodyAccJerk.std...X),
               testtbodyaccjerkstdy = mean(tBodyAccJerk.std...Y),
               testtbodyaccjerkstdz = mean(tBodyAccJerk.std...Z),
               testtbodygyromeanx = mean(tBodyGyro.mean...X),
               testtbodygyromeany = mean(tBodyGyro.mean...Y),
               testtbodygyromeany = mean(tBodyGyro.mean...Z),
               testtbodygyrostdx = mean(tBodyGyro.std...X),
               testtbodygyrostdy = mean(tBodyGyro.std...Y),
               testtbodygyrostdy = mean(tBodyGyro.std...Z),
               testtbodygyrojerkmeanx = mean(tBodyGyroJerk.mean...X), 
               testtbodygyrojerkmeany = mean(tBodyGyroJerk.mean...Y), 
               testtbodygyrojerkmeanz = mean(tBodyGyroJerk.mean...Z),
               testtbodygyrojerkstdx = mean(tBodyGyroJerk.std...X),
               testtbodygyrojerkstdy = mean(tBodyGyroJerk.std...Y),
               testtbodygyrojerkstdz = mean(tBodyGyroJerk.std...Z),
               testtbodyaccmagmean = mean(tBodyAccMag.mean..),
               testtbodyaccmagstd = mean(tBodyAccMag.std..),
               testtgravityaccmagmean = mean(tGravityAccMag.mean..),
               testtgravityaccmagstd = mean(tGravityAccMag.std..),
               testtbodyaccjerkmagmean = mean(tBodyAccJerkMag.mean..),
               testtbodyaccjerkmagstd = mean(tBodyAccJerkMag.std..),
               testtbodygyromagmean = mean(tBodyGyroMag.mean..),
               testtbodygyromagstd = mean(tBodyGyroMag.std..),
               testtbodygyrojerkmagmean = mean(tBodyGyroJerkMag.mean..),
               testtbodygyrojerkmagstd = mean(tBodyGyroJerkMag.std..),
               testfbodyaccmeanx =  mean(fBodyAcc.mean...X),
               testfbodyaccmeany = mean(fBodyAcc.mean...Y),
               testfbodyaccneanz = mean(fBodyAcc.mean...Z),
               testfbodyaccstdx = mean(fBodyAcc.std...X),
               testfbodyaccstdy = mean(fBodyAcc.std...Y),
               testfbodyaddstdz = mean(fBodyAcc.std...Z),
               testfbodyaccjerkmeanx = mean(fBodyAccJerk.mean...X),
               testfbodyaccjerkmeany = mean(fBodyAccJerk.mean...Y),
               testfbodyaccjerkmeanz = mean(fBodyAccJerk.mean...Z),
               testfbodyaccjerkstdx = mean(fBodyAccJerk.std...X),
               testfbodyaccjerkstdy = mean(fBodyAccJerk.std...Y),
               testfbodyaccjerkstdz = mean(fBodyAccJerk.std...Z),
               testfbodygyromeanx = mean(fBodyGyro.mean...X),
               testfbodygyromeany = mean(fBodyGyro.mean...Y),
               testfbodygyromeanz = mean(fBodyGyro.mean...Z),
               testfbodygyrostdx = mean(fBodyGyro.std...X),
               testfbodygyrostdy = mean(fBodyGyro.std...Y),
               testfbodygyrostdz = mean(fBodyGyro.std...Z),
               testfbodyaccmagmean = mean(fBodyAccMag.mean..),
               testfbodyaccmagstd = mean(fBodyAccMag.std..),
               testfbodyaccjerkmagmean = mean(fBodyBodyAccJerkMag.mean..),
               testfbodyaccjerkmagstd = mean(fBodyBodyAccJerkMag.std..),
               testfbodygyromagmean = mean(fBodyBodyGyroMag.mean..),
               testfbodygyromagstd = mean(fBodyBodyGyroMag.std..),
               testfbodygyrojerkmagmean = mean(fBodyBodyGyroJerkMag.mean..),
               testfbodygyrojerkmagstd = mean(fBodyBodyGyroJerkMag.std..),
               testangletbodyaccmeangravity = mean(angle.tBodyAccMean.gravity.),
               testangletbodyaccjerkmeangravitymean = mean(angle.tBodyAccJerkMean..gravityMean.),
               testangletbodygyromeangravitymean = mean(angle.tBodyGyroMean.gravityMean.),
               testangletbodygyrojerkmeangravitymean = mean(angle.tBodyGyroJerkMean.gravityMean.),
               testanglexgravitymean = mean(angle.X.gravityMean.),
               testangleygravitymean = mean(angle.Y.gravityMean.),
               testanglezgravitymean = mean(angle.Z.gravityMean.))
traindata1 <- traindata %>% group_by(activityid,subjectid) %>% 
     summarize(traintbodyaccmeanx = mean(tBodyAcc.mean...X), 
               traintbodyaccmeany = mean(tBodyAcc.mean...Y), 
               traintbodyaccneanz = mean(tBodyAcc.mean...Z),
               traintbodyaccstdx = mean(tBodyAcc.std...X), 
               traintbodyaccstdy = mean(tBodyAcc.std...Y), 
               traintbodyaccstdz = mean(tBodyAcc.std...Z), 
               traintgravityaccmeanx = mean(tGravityAcc.mean...X), 
               traintgravityaccmeany = mean(tGravityAcc.mean...Y), 
               traintgravityaccmeanz = mean(tGravityAcc.mean...Z), 
               traintgravitystdx = mean(tGravityAcc.std...X), 
               traintgravityaccstdy = mean(tGravityAcc.std...Y), 
               traintgravityaccstdz = mean(tGravityAcc.std...Z),
               traintbodyaccjerkmeanx = mean(tBodyAccJerk.mean...X), 
               traintbodyaccjerkmeany = mean(tBodyAccJerk.mean...Y), 
               traintbodyaccjerkmeanz = mean(tBodyAccJerk.mean...Z),
               traintbodyaccjerkstdx = mean(tBodyAccJerk.std...X),
               traintbodyaccjerkstdy = mean(tBodyAccJerk.std...Y),
               traintbodyaccjerkstdz = mean(tBodyAccJerk.std...Z),
               traintbodygyromeanx = mean(tBodyGyro.mean...X),
               traintbodygyromeany = mean(tBodyGyro.mean...Y),
               traintbodygyromeany = mean(tBodyGyro.mean...Z),
               traintbodygyrostdx = mean(tBodyGyro.std...X),
               traintbodygyrostdy = mean(tBodyGyro.std...Y),
               traintbodygyrostdy = mean(tBodyGyro.std...Z),
               traintbodygyrojerkmeanx = mean(tBodyGyroJerk.mean...X), 
               traintbodygyrojerkmeany = mean(tBodyGyroJerk.mean...Y), 
               traintbodygyrojerkmeanz = mean(tBodyGyroJerk.mean...Z),
               traintbodygyrojerkstdx = mean(tBodyGyroJerk.std...X),
               traintbodygyrojerkstdy = mean(tBodyGyroJerk.std...Y),
               traintbodygyrojerkstdz = mean(tBodyGyroJerk.std...Z),
               traintbodyaccmagmean = mean(tBodyAccMag.mean..),
               traintbodyaccmagstd = mean(tBodyAccMag.std..),
               traintgravityaccmagmean = mean(tGravityAccMag.mean..),
               traintgravityaccmagstd = mean(tGravityAccMag.std..),
               traintbodyaccjerkmagmean = mean(tBodyAccJerkMag.mean..),
               traintbodyaccjerkmagstd = mean(tBodyAccJerkMag.std..),
               traintbodygyromagmean = mean(tBodyGyroMag.mean..),
               traintbodygyromagstd = mean(tBodyGyroMag.std..),
               traintbodygyrojerkmagmean = mean(tBodyGyroJerkMag.mean..),
               traintbodygyrojerkmagstd = mean(tBodyGyroJerkMag.std..),
               trainfbodyaccmeanx =  mean(fBodyAcc.mean...X),
               trainfbodyaccmeany = mean(fBodyAcc.mean...Y),
               trainfbodyaccneanz = mean(fBodyAcc.mean...Z),
               trainfbodyaccstdx = mean(fBodyAcc.std...X),
               trainfbodyaccstdy = mean(fBodyAcc.std...Y),
               trainfbodyaddstdz = mean(fBodyAcc.std...Z),
               trainfbodyaccjerkmeanx = mean(fBodyAccJerk.mean...X),
               trainfbodyaccjerkmeany = mean(fBodyAccJerk.mean...Y),
               trainfbodyaccjerkmeanz = mean(fBodyAccJerk.mean...Z),
               trainfbodyaccjerkstdx = mean(fBodyAccJerk.std...X),
               trainfbodyaccjerkstdy = mean(fBodyAccJerk.std...Y),
               trainfbodyaccjerkstdz = mean(fBodyAccJerk.std...Z),
               trainfbodygyromeanx = mean(fBodyGyro.mean...X),
               trainfbodygyromeany = mean(fBodyGyro.mean...Y),
               trainfbodygyromeanz = mean(fBodyGyro.mean...Z),
               trainfbodygyrostdx = mean(fBodyGyro.std...X),
               trainfbodygyrostdy = mean(fBodyGyro.std...Y),
               trainfbodygyrostdz = mean(fBodyGyro.std...Z),
               trainfbodyaccmagmean = mean(fBodyAccMag.mean..),
               trainfbodyaccmagstd = mean(fBodyAccMag.std..),
               trainfbodyaccjerkmagmean = mean(fBodyBodyAccJerkMag.mean..),
               trainfbodyaccjerkmagstd = mean(fBodyBodyAccJerkMag.std..),
               trainfbodygyromagmean = mean(fBodyBodyGyroMag.mean..),
               trainfbodygyromagstd = mean(fBodyBodyGyroMag.std..),
               trainfbodygyrojerkmagmean = mean(fBodyBodyGyroJerkMag.mean..),
               trainfbodygyrojerkmagstd = mean(fBodyBodyGyroJerkMag.std..),
               trainangletbodyaccmeangravity = mean(angle.tBodyAccMean.gravity.),
               trainangletbodyaccjerkmeangravitymean = mean(angle.tBodyAccJerkMean..gravityMean.),
               trainangletbodygyromeangravitymean = mean(angle.tBodyGyroMean.gravityMean.),
               trainangletbodygyrojerkmeangravitymean = mean(angle.tBodyGyroJerkMean.gravityMean.),
               trainanglexgravitymean = mean(angle.X.gravityMean.),
               trainangleygravitymean = mean(angle.Y.gravityMean.),
               trainanglezgravitymean = mean(angle.Z.gravityMean.))

## Merge the two datasets
alldata <- merge(testdata1,traindata1,by.x = c("activityid","subjectid"),
                 by.y = c("activityid","subjectid"), all = TRUE)
## Merge in the Activities
alldata <- tbl_df(merge(alldata,activities,by.x = "activityid", by.y = "activityid"))

## Create the final summary and the file for submsission
finaltable <- select(alldata,activitylabel,subjectid:trainanglezgravitymean)
write.table(finaltable,"wearable_analysis_cmendoza.txt", row.names = FALSE)