## Load packages

library(dplyr)
library(stringr)

## Create objects with features and subjects

features <- read.table("features.txt")
featureslist <- features$V2

subjecttest <- read.table("subject_test.txt")
subjecttrain <- read.table("subject_train.txt")

## Create object with all test and train sections

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", 
                     col.names = featureslist)
y_test <-read.table("UCI HAR Dataset/testy_test.txt", header = FALSE)

x_train <-read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "",
                     col.names = featureslist)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

## Rename Columns

ytest <- rename(y_test, Activity = V1)
subjecttest <- rename(subjecttest, Subject = V1)
xtest <- select(x_test, contains("mean"), contains("std"),
                -contains("meanFreq"))
xtest <- xtest %>% rename_all(funs(str_replace
                  (., "Acc", "Acceleration")))
xtest <- xtest %>% rename_all(funs(str_replace
                        (., "Mag", "Magnitude")))
xtest <- xtest %>% rename_all(funs(str_replace
                            (., "std", "SD")))
xtest <- xtest %>% rename_all(funs(str_replace
                                   (., "mean", "Mean")))
xtest <- xtest %>% rename_all(funs(str_replace
                                   (., "anglet", "AngleTime")))
xtest <- xtest %>% rename_all(funs(str_replace
                                   (., "BodyBody", "Body")))
xtest <- xtest %>% rename_at(vars(starts_with("t")),
        funs(str_replace(., "t", "Time")))   
xtest <- xtest %>% rename_at(vars(starts_with("f")),
              funs(str_replace(., "f", "Freq")))

names(xtest) <- gsub("\\.", "", names(xtest))

## Rename Columns for train

ytrain <- rename(y_train, Activity = V1)
subjecttrain <- rename(subjecttrain, Subject = V1)
xtrain <- select(x_train, contains("mean"), contains("std"),
                -contains("meanFreq"))
xtrain <- xtrain %>% rename_all(funs(str_replace
                  (., "Acc", "Acceleration")))
  xtrain <- xtrain %>% rename_all(funs(str_replace
                    (., "Mag", "Magnitude")))
    xtrain <- xtrain %>% rename_all(funs(str_replace
                        (., "std", "SD")))
        xtrain <- xtrain %>% rename_all(funs(str_replace
                              (., "mean", "Mean")))
xtrain <- xtrain %>% rename_all(funs(str_replace
                    (., "anglet", "AngleTime")))
    xtrain <- xtrain %>% rename_all(funs(str_replace
                        (., "BodyBody", "Body")))
         xtrain <- xtrain %>% rename_at(vars(starts_with("t")),
                     funs(str_replace(., "t", "Time"))) 
            xtrain <- xtrain %>% rename_at(vars(starts_with("f")),
                        funs(str_replace(., "f", "Freq")))
                                   
names(xtrain) <- gsub("\\.", "", names(xtrain))

## Rename Activty Labels

ytest$Activity <- factor(ytest$Activity,
          levels = c(1,2,3,4,5,6),
          labels = c("walkking", "walking upstairs",
              "walking downstairs", "sitting", "standing",
                "laying"))

## Rename Activity labels Train

ytrain$Activity <- factor(ytrain$Activity,
                         levels = c(1,2,3,4,5,6),
                         labels = c("walking", "walking upstairs",
                                    "walking downstairs", "sitting", "standing",
                                    "laying"))

## Create object with all test sections

testdata <- cbind(subjecttest, ytest, xtest)

## Create object with all train sections

traindata <- cbind(subjecttrain, ytrain, xtrain)


## Merge the test data and the train data

fulldata <- merge(testdata, traindata, all = TRUE)


## Create new dataset with mean each variable by subject and activity

newdata <- group_by(fulldata,Activity, Subject)

finaldatamean <- summarise_each(newdata, funs(mean))