#Read and Merge X
dataset_training <- read.table("train/X_train.txt")
dataset_testing <- read.table("test/X_test.txt")
merged1 <- rbind(dataset_training, dataset_testing)

#Read and Merge Y
dataset_training <- read.table("train/y_train.txt")
dataset_testing <- read.table("test/y_test.txt")
merged2 <- rbind(dataset_training, dataset_testing)

#Read and Merge subject
dataset_training <- read.table("train/subject_train.txt")
dataset_testing <- read.table("test/subject_test.txt")
merged3 <- rbind(dataset_training, dataset_testing)

#Read feature and merge
dataset_feature <- read.table("features.txt")
index_feature <- grep("-mean\\(\\)|-std\\(\\)", dataset_feature[, 2])
merged1 <- merged1[, index_feature]
names(merged1) <- dataset_feature[index_feature, 2]
names(merged1) <- gsub("\\(|\\)", "", names(merged1))
names(merged1) <- tolower(names(merged1))

#Read Activity
activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
merged2[, 1] = activity[merged2[ , 1], 2]
names(merged2) <- "activity"
names(merged3) <- "subject"
clean <- cbind(merged3, merged2, merged1)

#Write
write.table(clean, "merged_clean_and_tidy_data.txt")

#Do exercise
merged_all <- unique(merged3)[, 1]
merged_unique <- length(unique(merged3)[, 1])
num_activities <- length(activity[, 1])
num_columns <- dim(clean)[2]
result <- clean[1:(merged_unique*num_activities), ]

row <- 1
for (s in 1:merged_unique){
  for (a in 1:num_activities){
    result[row, 1] <- merged_all[s]
    result[row, 2] <- activity[a, 2]
    temp <- clean[clean$subject == s & clean$activity == activity[a, 2], ]
    result[row, 3:num_columns] <- colMeans(temp[, 3:num_columns])
    row <- row + 1
  }
}

#Write
write.table(result, "data_set_with_the_averages.txt")