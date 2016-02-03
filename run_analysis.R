## Week Four Programming Assignment ###
## Getting and Cleaning Data ##
## David Risius ##
##This R Script reads both the test and training data set, merges them, reshapes
### the data set and produces a "tidy" data set that shows the average of each 
### movement for each; subject, activity, and type of movement.

library(dplyr)
setwd("~/Data_Science_Specialization/3_Getting&CleaningData/data/UCI HAR Dataset")

#first read the training data set
tr_xvals <- read.table("train/X_train.txt")
tr_yvals <- read.table("train/Y_train.txt")
features <- read.table('features.txt')
tr_subject <- read.table("train/subject_train.txt")


colnames(tr_xvals) <- features[,2]
tr_yvals <- tr_yvals[,1]
#question 3 adds the labels to each activity
tr_yvals[tr_yvals==1]="walk"
tr_yvals[tr_yvals==2]="walkup"
tr_yvals[tr_yvals==3]="walkdown"
tr_yvals[tr_yvals==4]="sitting"
tr_yvals[tr_yvals==5]="standing"
tr_yvals[tr_yvals==6]="laying"

tr_xvals$subject <- tr_subject[,1]
tr_xvals$activity <- tr_yvals

train <- tr_xvals #training data set with all the labels and activities added

#next the test data set
te_xvals <- read.table("test/X_test.txt")
te_yvals <- read.table("test/Y_test.txt")
te_subject <- read.table("test/subject_test.txt")


colnames(te_xvals) <- features[,2]
#adds the labels to each activity
te_yvals <- te_yvals[,1]
te_yvals[te_yvals==1]="walk"
te_yvals[te_yvals==2]="walkup"
te_yvals[te_yvals==3]="walkdown"
te_yvals[te_yvals==4]="sitting"
te_yvals[te_yvals==5]="standing"
te_yvals[te_yvals==6]="laying"

te_xvals$subject <- te_subject[,1]
te_xvals$activity <- te_yvals

test <- te_xvals #test data set with all the labels and activities added


# 1. Merge the training and the test sets to create one data set.

#now merge the two data frames.  Since they are the same column dimensions, we will merge
intersect(names(test), names(train))
train$Group <- "train" #so we know which is the training and the test
test$Group <- "test"
all <- rbind(train, test) #creates the merged data set

# 2. Extracts only the measurements on the mean and standard deviation for each 
# measurement.

x <- grep("meanFreq()", names(all)) 
all <- all[,-(x)] #takes out meanFreq() columns which were screwing up the data
names(all)
y <- grep("mean()|std()", names(all))
all <- all[,c(y, 549:551 ) ] #uses only those columns with the mean and standard deviation
names(all)

# 3. Uses descriptive activity names to name the activities in the data set

  # completed in the initial step after I read in the data

# 4. Appropriately labels the data set with descriptive variable names.
library(reshape2) #will use to reshape the data set

#create the new 'skinny' dataset with descriptive column names
allMelt <- melt(all, id.vars = c("subject", "activity", "Group"), 
                measure.vars = c(names(all)[1:66]), variable.name = "movement")

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
allSummary <- aggregate(value ~ subject+activity+movement, data = allMelt, 
                        FUN = "mean")

