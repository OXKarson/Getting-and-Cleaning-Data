library(data.table)

readData <- function(columns) {
  te <- fread("UCI HAR Dataset/test/X_test.txt", select = columns, data.table = FALSE)
  tr <- fread("UCI HAR Dataset/train/X_train.txt", select = columns, data.table = FALSE)
  return(rbind(te, tr))
}
downloadData <- function() {
  if (!file.exists("UCI HAR Dataset/features.txt")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "HARDataset.zip", "curl")
    unzip("HARDataset.zip")
  }
}
getCleanedHAR <- function() {
  columnNames <- read.table("UCI HAR Dataset/features.txt")[,2]
  filterColumnNames <- grep("mean|std", columnNames)
  data <- readData(filterColumnNames)
  colnames(data) <- columnNames[filterColumnNames]
  activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")[,2]
  activities <- c(read.table("UCI HAR Dataset/test/y_test.txt")[,1], read.table("UCI HAR Dataset/train/y_train.txt")[,1])
  data$subject <- c(read.table("UCI HAR Dataset/test/subject_test.txt")[,1], read.table("UCI HAR Dataset/train/subject_train.txt")[,1])
  data$activity <- activityLabels[activities]
  return(data)
}
getTidyDataset <- function(har) {
  # create factor & split har dataset
  subjectActivityFactor <- paste0(har$subject, har$activity)
  s <- split(har[,1:grep("subject", names(har))-1], subjectActivityFactor)
  tidyDataset <- sapply(s, colMeans)
  tidyDataset <- as.data.frame(t(tidyDataset)) # turn dataset
  # convert from factor to data
  subjectActivity <- har[,c("subject", "activity")][!duplicated(subjectActivityFactor),]
  rownames(subjectActivity) <- unique(subjectActivityFactor)
  tidyDataset$subject <- subjectActivity[rownames(tidyDataset),]$subject
  tidyDataset$activity <- subjectActivity[rownames(tidyDataset),]$activity
  # reorder dataset
  tidyDataset <- tidyDataset[c("subject", "activity", colnames(tidyDataset)[1:grep("subject", names(har))-1])]
  # remove rownames
  rownames(tidyDataset) <- NULL
  return(tidyDataset)
}
generateCleanedDataSets <- function() {
  downloadData()
  har <- getCleanedHAR()
  write.table(har, "HAR.txt", row.names = FALSE)
  tidyDataset <- getTidyDataset(har)
  write.table(tidyDataset, "tidyHAR.txt", row.names = FALSE)
}

generateCleanedDataSets()