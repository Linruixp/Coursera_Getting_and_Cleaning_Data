# Read the data into R
# assuming the R working directory has been properly set
train <- read.table("./train/X_train.txt", head = FALSE, sep = "")
test <- read.table("./test/X_test.txt", head = FALSE, sep = "")

# Step 1 - Merges the training and the test sets to create one data set.
data <- rbind(train, test)

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# reads features.txt into R
features <- read.table("features.txt", head = FALSE, sep = "")
# selecting column names(measurenments) whose name with "mean()" or "std()", 
# storing thier number into a vector called filter
filter <- c(grep("mean\\(\\)", features$V2), grep("std\\(\\)", features$V2))
filter <- sort(filter)
# extracts variables from data set which match the filter 
filtered_vars <- paste("V", filter, sep="")
extracted_data <- data[filtered_vars]
# filter the features vector too
extracted_features <- features[filter,]

# Step 3 - Uses descriptive activity names to name the activities in the data set
# read the training  and test labels into R, and combined them into a data frame which is named 'labels'
y_train <- read.table("./train/y_train.txt", head = FALSE, sep = "")
y_test <- read.table("./test/y_test.txt", head = FALSE, sep = "")
labels <- rbind(y_train, y_test)
# read activity names into R
activity_names <- read.table("activity_labels.txt", head = FALSE, sep = "")
# attach activity names by merge labels and actitity_names
# codes below will use join method of 'plyr' package
install.packages("plyr")
library(plyr)
labels <- join(labels, activity_names, by = "V1", type = "left")
# name the activities in the data set by adding a new variable 'activity'
extracted_data$activity <- labels$V2

# Step 4 - Appropriately labels the data set with descriptive variable names.
# the second variable V2 of data frame 'features' contains the descriptive variable names
# codes below will use setnames method of 'data.table' package
install.packages("data.table")
library(data.table)
setnames(extracted_data, old = colnames(extracted_data)[1:length(filter)], new = as.character(extracted_features$V2))

# Step 5 - Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# read subjects
subject_train <- read.table("./train/subject_train.txt" ,head = FALSE, sep = "")
subject_test <- read.table("./test/subject_test.txt" ,head = FALSE, sep = "")
subject <- rbind(subject_train, subject_test)
# link subjects with observations by adding a new variable 'subject' to data set
extracted_data$subject <- subject[[1]]
# get mean of each variables in each categorty (group by actiity*subject)
aggdata <- aggregate(extracted_data[1:length(filter)], by=list(extracted_data$subject, extracted_data$activity), FUN=mean)
# remove weird characters like "()" "-"
setnames(aggdata, old = colnames(aggdata), new = gsub("\\(|\\)", "", colnames(aggdata)) ) # remove "()"
setnames(aggdata, old = colnames(aggdata), new = gsub("-", ".", colnames(aggdata)) ) # replace "-" with "."
# rename the first and second variables(Group.1 and Group.2)  to subject and activity, and get tidy data
tidy_data <- rename(aggdata, c("Group.1"="subject", "Group.2"="activity"))


# Writes tidy data to a file for uploading
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)


