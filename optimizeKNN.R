# Kaggle: Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data

#library(class)   # "recommended" k-Nearest Neighbour Classification: TOO SLOW
library(FNN)    # Fast Nearest Neighbor Search Algorithms and Applications

# load the data
rawTrainData <- read.csv("train.csv", header=TRUE)#[1:25000,]
#test <- read.csv("test.csv", header=TRUE)[1:1000,]

# randomly permute row order of training dataset
train <- rawTrainData[sample(nrow(rawTrainData)), ]
# then split up into training and cross-validation sets
cv <- train[(1+(0.6*nrow(train))):nrow(train), ]   # 2 needed instead of 1 to avoid overlap row?
train <- train[1:(0.6*nrow(train)), ]

# drop label columns for use in KNN
trainCl <- train[, 1]
train <- train[, -1]
cvCl <- cv[, 1]
cv <- cv[, -1]

# what about removing pixels with near zero variance? not good predictors...
library(caret)
badCols <- nearZeroVar(train)
print(paste("Fraction of nearZeroVar columns:", length(badCols)/length(train)))
train <- train[, -badCols]
cv <- cv[, -badCols]
            
# fit knn model to training set
# get knn predictions for training set
# compute fractional training error
# for numK values of k
numK <- 6
trainErrs <- numeric(numK)
for(i in 1:numK) {
    print(paste("Train:", i))
    trainModel <- (0:9)[knn(train, train, trainCl, k=i, algorithm="cover_tree")]
    #trainPreds <- as.numeric(levels(trainModel))[trainModel]
    trainPreds <- trainModel
    trainErr <- length(which(trainCl != trainPreds)) / length(trainPreds)
    trainErrs[i] <- trainErr
    }

# repeat for cross-validation set
cvErrs <- numeric(numK)
for(i in 1:numK) {
    print(paste("CV:", i))
    cvModel <- (0:9)[knn(train, cv, trainCl, k=i, algorithm="cover_tree")]
    #cvPreds <- as.numeric(levels(cvModel))[cvModel]
    cvPreds <- cvModel
    cvErr <- length(which(cvCl != cvPreds)) / length(cvPreds)
    cvErrs[i] <- cvErr
}

# plot training errors as a function of k
plot(trainErrs, type='p', col='blue', ylim=c(0.0,0.1), pty="s",
     xlab="Number of Nearest Neighbors", ylab="Fractional Error", main="Optimizing kNN Model")
trainCurve <- loess(trainErrs ~ c(1:numK))
lines(predict(trainCurve), col='blue', lwd=2)

# plot cross-validation errors as a function of k
points(cvErrs, type='p', col='red')
cvCurve <- loess(cvErrs ~ c(1:numK))
lines(predict(cvCurve), col='red', lwd=2)

legend("bottomright", legend=c("Cross-Validation", "Training"),
       col=c("red", "blue"),lwd=2, bty="n", y.intersp=1.5, inset=0.02)
