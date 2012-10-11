# Kaggle: Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data
# produce submission file with optimal knn model

# load training and test datasets
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

##########################################################
# Fast Nearest Neighbor Search Algorithms and Applications
library(FNN)

# drop label columns for use in KNN
trainCl <- train[, 1]
train <- train[, -1]

# remove pixels with near zero variance -- not good predictors
library(caret)
badCols <- nearZeroVar(train)
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(train),4)))
train <- train[, -badCols]
test <- test[, -badCols]

# train the knn model
results <- (0:9)[knn(train, test, trainCl, k=5, algorithm="cover_tree")]

# save the output as column vector
write(results, file="knn_submission.csv", ncolumns=1)