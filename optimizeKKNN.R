# Kaggle: Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data
# optimize a knn model using the kknn implementation

# Weighted k-Nearest Neighbors
library(kknn)

# load the training data
rawTrainData <- read.csv("train.csv", header=TRUE)

# randomly sample rows of training data
train <- rawTrainData[sample(nrow(rawTrainData)), ]
train <- train[1:10000,]

# optimize knn for k and kernel
# using leave-one-out cross-validation
kMax <- 15
kernels <- c("triangular","rectangular","gaussian")
model_1 <- train.kknn(as.factor(label) ~ ., train, kmax=kMax, kernel=kernels)

# what about removing pixels with near zero variance? not good predictors...
library(caret)
badCols <- nearZeroVar(train[,-1])
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(train),4)))
train <- train[, -(badCols+1)]
model_2 <- train.kknn(as.factor(label) ~ ., train, kmax=kMax, kernel=kernels)

# what about centering and scaling (standardizing)?
# UPDATE: This gives identical results to model_2
#train <- rawTrainData[sample(nrow(rawTrainData)), ]
#train <- train[1:1000,]
#label <- train[, 1]
#centeredPixels <- apply(train[, -1], 2, scale, center=TRUE, scale=TRUE)
#train <- data.frame(label, centeredPixels)
#model_3 <- train.kknn(as.factor(label) ~ ., train, kmax=kMax, kernel=kernels)

#######################################################################
# plot mis-classification rate per k, per kernel, per data modification
plot(1:nrow(model_1$MISCLASS), model_1$MISCLASS[,1], type='n', col='blue', ylim=c(0.0,0.105),
     xlab="Number of Nearest Neighbors", ylab="Fractional Error Rate", main="kNN performance by k and kernel")
# model_1
for(kern in kernels) {
    color=rainbow(length(kernels))[match(kern, kernels)]
    points(1:nrow(model_1$MISCLASS), model_1$MISCLASS[,kern], type='p', pch=16, col=color)
    lines(predict(loess(model_1$MISCLASS[,kern] ~ c(1:kMax))), col=color, lwd=2, lty="solid")
}
# model_2
for(kern in kernels) {
    color=rainbow(length(kernels))[match(kern, kernels)]
    points(1:nrow(model_2$MISCLASS), model_2$MISCLASS[,kern], type='p', pch=17, col=color)
    lines(predict(loess(model_2$MISCLASS[,kern] ~ c(1:kMax))), col=color, lwd=2, lty="dotted")
}
# model_3
#for(kern in kernels) {
#    color=rainbow(length(kernels))[match(kern, kernels)]
#    points(1:nrow(model_3$MISCLASS), model_3$MISCLASS[,kern], type='p', pch=15, col=color)
#    lines(predict(loess(model_3$MISCLASS[,kern] ~ c(1:kMax))), col=color, lwd=2, lty="dotted")
#}

# mark the best values of each set of models
model_1_best <- model_1$MISCLASS[model_1$best.parameters$k, model_1$best.parameters$kernel]
model_2_best <- model_2$MISCLASS[model_2$best.parameters$k, model_2$best.parameters$kernel]
points(model_1$best.parameters$k, model_1_best, pch=16, col="black")
points(model_2$best.parameters$k, model_2_best, pch=17, col="black")

legend("bottomright", ncol=2, legend=c(kernels, paste(kernels,"(red. data)")),
       col=rep(rainbow(length(kernels)),2), pch=c(rep(16,3), rep(17,3)),
       lwd=2, lty=c(rep("solid",3), rep("dotted",3)),
       bty="n", y.intersp=1.5, inset=0.01, cex=0.8)


# print out the best parameters
print(paste("Best model_1 parameters:", "kernel =", model_1$best.parameters$kernel, ", k =", model_1$best.parameters$k))
print(model_1$MISCLASS)
print(paste("Best model_2 parameters:", "kernel =", model_2$best.parameters$kernel, ", k =", model_2$best.parameters$k))
print(model_2$MISCLASS)
#print(paste("Best model_3 parameters:", "kernel =", model_3$best.parameters$kernel, ", k =", model_3$best.parameters$k))
#print(model_3$MISCLASS)