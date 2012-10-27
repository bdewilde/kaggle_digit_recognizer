# Kaggle: Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data
# optimize a knn model using the FNN implementation

# Fast Nearest Neighbor Search Algorithms and Applications
library(FNN)

# load the data
rawTrainData <- read.csv("train.csv", header=TRUE)[1:1000,]

# randomly sample rows of training data
indices <- sample(1:nrow(rawTrainData), trunc(0.6*nrow(rawTrainData)))
# assign training and cross-validation sets to orthogonal sub-sets
train <- rawTrainData[indices, ]
cv <- rawTrainData[-indices, ]

# drop label columns for use in KNN
trainCl <- train[, 1]
train <- train[, -1]
cvCl <- cv[, 1]
cv <- cv[, -1]

# what about removing pixels with near zero variance? not good predictors...
library(caret)
badCols <- nearZeroVar(train)
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(train),4)))
train <- train[, -badCols]
cv <- cv[, -badCols]

# what about centering and scaling? i.e. standardizing
trainColMeans <- apply(train, 2, mean)
trainColSD <- apply(train, 2, sd)
train <- apply(train, 2, scale, center=TRUE, scale=TRUE)
#cv <- apply(cv, 2, scale, center=trainColMeans, scale=trainColSD)
#train <- apply(train, 2, scale, center=TRUE, scale=TRUE)
#cv <- apply(cv, 2, scale, center=TRUE, scale=TRUE)
cv <- sweep(cv, 2, trainColMeans, FUN="-")
cv <- sweep(cv, 2, trainColSD, FUN="/")

# fit knn model to training set
# get knn predictions for training set
# compute fractional training error
# for numK values of k
numK <- 10
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
