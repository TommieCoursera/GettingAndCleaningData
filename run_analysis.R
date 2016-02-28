# ***************************************************
# 1. Merges the training and the test sets to create one data set.
# ***************************************************
# Read data from X dataset
x.train <- read.table("./train/X_train.txt")
x.test <- read.table("./test/X_test.txt")

# read data from y label dataset
y.train <- read.table("./train/y_train.txt")
y.test <- read.table("./test/y_test.txt")
summary(y.train)

# read data from subject, features, and activity labels dataset
subject.test <- read.table("./test/subject_test.txt")
subject.train <- read.table("./train/subject_train.txt")
features <- read.table("./features.txt")
activity.labels <- read.table("./activity_labels.txt")

# merge subject
subject <- rbind(subject.test, subject.train)
colnames(subject) <- "subject"
summary(subject)

# merge label dataset
ylabel <- rbind(y.train, y.test)
ylabel <- merge(ylabel, activity.labels, by.x = "V1", by.y = "V1")[,2]

# merge data
data <- rbind(x.train, x.test)
colnames(data) <- features[,2]
summary(data)

# merge subject, label, and data
data <- cbind(subject, ylabel, data)
colnames(data)

# ***************************************************
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# ***************************************************
# retrieve data by searching all "mean" or "std" columns
extract <- grep("-mean|-std", colnames(data)) 
data.meanstd <- data[, c(1, 2, extract)] # col1: subject, col2: ylabel
colnames(data.meanstd)

# ***************************************************
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# ***************************************************
library(dplyr)

# calculate mean for all variables by (activity)
activity.mean <- data.meanstd %>%
  group_by(ylabel) %>%
  select(3:length(data.meanstd)) %>%  #Skip column 1/column 2 (subject/label) before calculation
  summarize_each(funs(mean)) %>%
  arrange(ylabel)

# calculate mean for all variables by (subject)
subject.mean <- data.meanstd %>%
  group_by(subject) %>%
  select(3:length(data.meanstd)) %>%  #Skip column 1/column 2 (subject/label) before calculation
  summarize_each(funs(mean)) %>%
  arrange(subject)

# calculate mean for all variables by (activity & subject)
activity.subject.mean <- data.meanstd %>%
  group_by(ylabel, subject) %>%
  select(3:length(data.meanstd)) %>%  #Skip column 1/column 2 (subject/label) before calculation
  summarize_each(funs(mean)) %>%
  arrange(ylabel, subject)

# write final result for Step #5
write.table(activity.subject.mean, file = "tidy_dataset.txt", row.name = FALSE)
# write final result for Step #5 on the screen
activity.subject.mean

