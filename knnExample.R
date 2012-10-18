# bi-variate normal distributions
library(MASS)
a <- mvrnorm(n=100, mu=c(0.0, 1.5), Sigma=matrix(c(1,0,0,1), nrow=2, ncol=2))
b <- mvrnorm(n=100, mu=c(1.5, 0.0), Sigma=matrix(c(1,0,0,1), nrow=2, ncol=2))
a <- cbind(a, 0)
b <- cbind(b, 1)
colnames(a) <- c('x1','x2','class')
colnames(b) <- c('x1','x2','class')
train <- as.data.frame(rbind(a, b))

library(lattice)
with(train, xyplot(x2~x1, groups=class, col=c("gold", "darkorchid"), pch=19))

px1 <- seq(from=min(train$x1), to=max(train$x1), by=0.05)
px2 <- seq(from=min(train$x2), to=max(train$x2), by=0.05)
test <- expand.grid(x1=px1, x2=px2)

# train the knn model
library(class)
numK <- 25
model <- knn(train=train[,1:2], test=test, cl=train[,3], k=numK, prob=TRUE)
prob <- attr(model, "prob")
prob <- ifelse(model==1, prob, 1-prob)
probMat <- matrix(prob, nrow=length(px1), ncol=length(px2))

# plot the results
contour(px1, px2, probMat, levels=0.5, lwd=2, labels="", axes=FALSE,
        xlab="x1", ylab="x2", main=paste("Binary kNN Classification (k=", numK, ")", sep=""), mgp=c(1,1,0), cex.lab=1.5)
points(test, pch=".", col=ifelse(probMat>0.5, "darkorchid", "gold"))
points(a, col="gold", pch=20)
points(b, col="darkorchid", pch=20)
box()
