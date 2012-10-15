# Kaggle Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data

# plotting libraries
library(ggplot2)
library(reshape2)

# load training data
train <- read.csv("train.csv", header=TRUE)[1:1000, ]

# subset individual classes
subsets <- list()
rowMeans <- list()
whitePixels <- list()
for(digit in 0:9) {
    d <- as.character(digit)
    subsets[[d]] <- subset(train, train$label==as.character(digit))
    rowMeans[[d]] <- rowMeans(subsets[[d]])
    whitePixels[[d]] <- apply(subsets[[d]], 1, function(x) length(which(x=="0"))/length(x))
    }
boxplot(rowMeans)
boxplot(whitePixels, ylab="Fraction of White Pixels")

matricize <- function(vec) {
    mat <- matrix(vec[-1], nrow=28, ncol=28, byrow=TRUE)
    mat <- apply(mat, 1, as.numeric)
    return(mat)
    }

getMinMaxRowsAndCols <- function(mat) {
    cMeans <- which(colMeans(mat)!=0, arr.ind=TRUE)
    rMeans <- which(rowMeans(mat)!=0, arr.ind=TRUE)
    cMin <- min(cMeans); cMax <- max(cMeans)
    rMin <- min(rMeans); rMax <- max(rMeans)
    return(c(rMin, rMax, cMin, cMax))
    }

subsetMats <- list()
minMaxRowsAndCols <- list()
widths <- list()
heights <- list()
for(digit in 0:1) {
    d <- as.character(digit)
    subsetMats <- apply(subsets[[d]], 1, matricize)
    print(dim(subsetMats))
    #minMaxRowsAndCols <- apply(subsetMats, 1, getMinMaxRowsAndCols)
}

###########################################################
# randomly sample nPlot training digits (rows) for plotting
nPlot <- 1
trainSamp <- train[sample(1:nrow(train), nPlot), ]

# convert sampled row vectors into 28x28 numeric matrices
# dropping label column and transforming so as to plot with correct orientation (ugh)
# save matrices in a list
trainMats <- list()
for(i in 1:nrow(trainSamp)) {
    trainMat <- matrix(trainSamp[i,-1], nrow=28, ncol=28, byrow=TRUE)
    trainMat <- trainMat[nrow(trainMat):1,]
    trainMat <- apply(trainMat, 1, as.numeric)
    trainMats[[i]] <- trainMat
    }
# this didn't work:
# digits with same label values write on top of each other in same grid box
# apparently labels must be unique
#names(trainMats) <- c(trainSamp[,1])

# ggplot sampled digits in a (more or less) square grid of geom_tiles
# set theme to nearly-blank
ggplot(melt(trainMats), aes(x=Var1, y=Var2, fill=value)) +
    theme_bw() +
    facet_wrap(~ L1, ncol=sqrt(nPlot)) +
    geom_tile(aes(fill=value)) +
    coord_equal() +
    scale_fill_gradient(low="white",high="black") +
    ggtitle("MINST Database of Hand-written Digits") +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
         axis.text.y=element_blank(), axis.ticks=element_blank(),
         axis.title.x=element_blank(), axis.title.y=element_blank(),
         plot.title=element_text(size=20),
         legend.position="none",
         panel.background=element_blank(), panel.grid.major=element_blank(), #panel.border=element_blank(),
         panel.grid.minor=element_blank(), plot.background=element_blank(),
         strip.background=element_blank(), strip.text.x=element_blank()) #strip.text.x=element_text()
dev.copy(pdf, "digits.pdf")
dev.off()

# failed alternatives...
#heatmap(trainMat, Rowv=NA, Colv=NA, na.rm=FALSE, col=gray.colors(256, start=1.0, end=0.0, gamma=1.0))
#levelplot(trainMat, xlab="", ylab="", col.regions=gray.colors(256, start=1.0, end=0.0, gamma=1.0), colorkey=FALSE)
