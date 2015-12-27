run_analysis <- function () {
    # first we read the data from disk, we assume that the Samsung data is in our working directory
    # there are two data sets: test data, and training data
    
    library(dplyr) # needed for manipulation of data
    combinedData <- data.frame()
    
    # step 1 : subject files
    
    # first read the subject files
    subjectTrain <- read.table("train/subject_train.txt")
    subjectTest <- read.table("test/subject_test.txt")
    
    # merge the train and test
    subject <- rbind(subjectTrain, subjectTest)
    
    # step 2 : activity (y) files
    # add the y labels then merge them
    train_y <- read.table("train/y_train.txt")
    test_y <- read.table("test/y_test.txt")
    
    data_y <- rbind(train_y, test_y)
    
    combinedData <- as.data.frame(cbind(subject = subject$V1, activity = data_y$V1))
    
    ## give descriptive names to the activity column
    combinedData$activity <- as.factor(combinedData$activity)
    activityNames <- read.table('activity_labels.txt', stringsAsFactors = FALSE)[[2]]
    levels(combinedData$activity) <- activityNames
    
    # step 3 : features files
    # read the column names for the features first to have descriptive names
    features <- read.table("features.txt", stringsAsFactors = FALSE)
    featuresNames <- features$V2 ## the names are stored in the second column
    
    # read the data sets and merge them into one
    train_X <- read.table("train/x_train.txt", col.names = featuresNames)
    test_X <- read.table("test/X_test.txt", col.names = featuresNames)
    
    merged_x <- rbind(train_X, test_X)

    # now we only want to select columns related to the mean and std for each measurement
    selector <- grepl("mean()|std()" , featuresNames, ignore.case = TRUE) # we'll use that logical vector to index 
                                                             #the columns we want
    
    mean_and_std_data_x <- merged_x[,selector]
    
    # append to the main data frame
    combinedData <- cbind(combinedData, mean_and_std_data_x)
    
    # now we want to group the data set by subject and activity, and for each pair, calculate 
    # the mean of every measurement, we do this using dply's group_by() and summarise_each_()
    
    groupedData <- group_by(combinedData, subject, activity)
    tidyData <- summarise_each_(groupedData, funs(mean), names(groupedData))
}
