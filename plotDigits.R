# Kaggle Digit Recognizer
# https://www.kaggle.com/c/digit-recognizer/data

# plotting libraries
library(ggplot2)
library(reshape2)

# load training data
train <- read.csv("train.csv", header=TRUE)#[1:20000, ]

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
boxplot(rowMeans, ylab="Mean Pixel Brightness [0-255]", main="From the MINST Database of Hand-written Digits")
boxplot(whitePixels, ylab="Fraction of White Pixels", main="From the MINST Database of Hand-written Digits")

matricize <- function(row) {
    mat <- matrix(row[-1], nrow=28, ncol=28, byrow=TRUE)
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

# plot widths and heights of digits by class
# this is insanely inefficient!! *apply was not working
widths <- list()
heights <- list()
for(digit in 0:9) {
    d <- as.character(digit)
    widths[[d]] <- numeric()
    heights[[d]] <- numeric()
    for(i in 1:nrow(subsets[[d]])) {
        rowMat <- matricize(subsets[[d]][i,])
        minMaxRowsAndCols <- getMinMaxRowsAndCols(rowMat)
        width <- minMaxRowsAndCols[4]-minMaxRowsAndCols[3]
        height <- minMaxRowsAndCols[2]-minMaxRowsAndCols[1]
        widths[[d]] <- c(widths[[d]], width)
        heights[[d]] <- c(heights[[d]], height)
    }
}
# have i swapped widths and heights?
boxplot(widths, ylab="Digit Height [pixels]", ylim=c(0,28))
boxplot(heights, ylab="Digit Width [pixels]", ylim=c(0,28), main="From the MINST Database of Hand-written Digits")


###########################################################
# randomly sample nPlot training digits (rows) for plotting
nPlot <- 9

trainPermute <- train[sample(nrow(train)), ]
inds <- numeric()
for(i in 0:9) {
    inds <- c(inds, which(trainPermute$label==as.character(i))[1:10])
}
trainSamp <- rbind(trainPermute[inds,])
#trainSamp <- train[sample(1:nrow(train), nPlot), ]

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
    facet_wrap(~ L1, ncol=10) + # sqrt(nPlot)
    geom_tile(aes(fill=value)) +
    coord_equal() +
    scale_fill_gradient(low="white",high="black") +
    ggtitle("From the MINST Database of Hand-written Digits") +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
         axis.text.y=element_blank(), axis.ticks=element_blank(),
         axis.title.x=element_blank(), axis.title.y=element_blank(),
         plot.title=element_text(size=16),
         legend.position="none",
         panel.background=element_blank(), panel.grid.major=element_blank(), #panel.border=element_blank(),
         panel.grid.minor=element_blank(), plot.background=element_blank(),
         strip.background=element_blank(), strip.text.x=element_blank()) #strip.text.x=element_text()
dev.copy(pdf, "digits.pdf")
dev.off()

# failed alternatives...
#heatmap(trainMat, Rowv=NA, Colv=NA, na.rm=FALSE, col=gray.colors(256, start=1.0, end=0.0, gamma=1.0))
#levelplot(trainMat, xlab="", ylab="", col.regions=gray.colors(256, start=1.0, end=0.0, gamma=1.0), colorkey=FALSE)
