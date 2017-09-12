library("dplyr")
library("tidyr")
library("data.table")
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "har_ds.zip")
unzip(zipfile = "har_ds.zip")
#read files into a data frame
setwd("C:/Users/vik2v/Documents/R/UCI HAR Dataset/train")
dt_subject_train <- fread(input = "subject_train.txt", data.table = FALSE)
dt_x_train <- fread(input = "X_train.txt", data.table = FALSE)
dt_y_train <- fread(input = "Y_train.txt", data.table = FALSE)
setwd("C:/Users/vik2v/Documents/R/UCI HAR Dataset/test")
dt_subject_test <- fread(input = "subject_test.txt", data.table = FALSE)
dt_x_test <- fread(input = "X_test.txt", data.table = FALSE)
dt_y_test <- fread(input = "Y_test.txt", data.table = FALSE)
setwd("C:/Users/vik2v/Documents/R/UCI HAR Dataset")
dt_features <- fread(input = "features.txt", data.table = FALSE)
dt_activity <- fread(input = "activity_labels.txt", data.table = FALSE)

#merge training and test data sets to one data set
dt_subject <- bind_rows(dt_subject_train, dt_subject_test)
dt_x <- bind_rows(dt_x_train,dt_x_test)
dt_y <- bind_rows(dt_y_train,dt_y_test)
#check counts in all 3 sets returns same - 10299
#nrow(dt_subject)
#nrow(dt_x)
#nrow(dt_y)

#rename col headers
dt_subject <- dt_subject %>% rename(subject = V1)
dt_y <- dt_y %>% rename(activitynum = V1)

#build one dataset - subject, activitynum, V1...V561
dt_subject <- bind_cols(dt_subject, dt_y)
fdata <- bind_cols(dt_subject,dt_x)

#select features to get mean and std, fix column names
featureswanted <- dt_features %>% filter(grepl("mean\\(\\)|std\\(\\)",V2))
featureswanted <- rename(featureswanted, featurename = V2, feature_code = V1)
dt_activity <- rename(dt_activity, activitynum = V1, activityname = V2)

#convert to tidy data
#convert features to rows, calculate average by subject, activitynum, feature
fd <- gather(fdata,key = "featurecode", value = "measurement", c(V1:V561))
fd1 <- fd %>% group_by(subject,activitynum,featurecode) %>% summarize(Average = mean(measurement))
fd1 <- mutate(fd1, feature_code = gsub("V","",featurecode))
#add featurename, activityname
fd2 <- merge(fd1,featureswanted)
fd3 <- merge(fd2,dt_activity)
#remove duplicate columns such as featurecode
fd4 <- fd3 %>% select(subject,activityname,featurename,Average)
#clean up feature name, convert feature, average from rows to columns
fd5 <- fd4 %>% mutate(feature = gsub("-","",featurename)) %>% mutate(feature = gsub("\\()","",feature)) %>% select(-featurename) %>% spread(feature,Average)
#create tidy.txt
write.table(fd5, file = "tidy.txt",row.names = FALSE)

