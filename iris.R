

dat <- read.csv("iris.data", header = FALSE)
names(dat) <- c("sepal.length",
                "sepal.width",
                "petal.length",
                "petal.width",
                "class")

library(RColorBrewer)
palette(brewer.pal(6, "Paired"))

plot(dat[,-5], col = dat$class, pch = 16)

# initialize parameters
k <- 3
m <- dim(dat)[1]
dmat <- data.frame(matrix(rep(0, k*m),m,k)) #distance matrix

# initialize centroids to k random data points from set
# set.seed(3)
sam <- sample(1:m, k)
centroid <- dat[sam,-5]

# distance between two data points
dist <- function(x1, x2) (sum(x1 - x2))^2

# center of mass
cm <- function(dat) apply(dat[,-5], 2, mean)

# plot centroids and assigned cluster by color
update.plot <- function(dat, centroid, clust){
    k <- dim(centroid)[1]
    m <- dim(dat)[1]
    centroid$class <- "centroid"
    dm <- rbind(centroid, dat)
    clust <- c(1:k, clust)
    size <- c(rep(2,k), rep(1,m))
    plot(dm[,-5], col = clust, pch = 16, cex = size)
}

# assign each observation to a cluster based on distance to centroid
cluster <- function(dat, centroid){
    k <- dim(centroid)[1]
    m <- dim(dat)[1]
    for(i in 1:m){
        for(j in 1:k){
            dmat[i,j] <- dist(dat[i,-5], centroid[j,])
        }
    }
    clust <- as.factor(apply(dmat, 1, which.min))
}

# move centroid to center of mass of each cluster
update.centroid <- function(dat, clust){
    k <- nlevels(clust)
    m <- dim(dat)[1]
    centroid <- matrix(unlist(by(dat[,-5], clust, colMeans)), k, 4, byrow = TRUE)
    centroid <- as.data.frame(centroid)
    names(centroid) <- c("sepal.length",
                         "sepal.width",
                         "petal.length",
                         "petal.width")
    centroid
}

# calculate error
class.accuracy <- function(f1, f2) {
    library(e1071)
    k <- nlevels(f1)
    perm <- permutations(k)
    ## determine how to evaluate all possible factor permutations and use min
    acc <- 9e99
    for(i in 1:dim(perm)[1]){
        acc[i] <- sum(unclass(f1) == 
                           unclass(factor(f2, levels = perm[i,])))/length(f1)
    }
    accout <- max(acc)
    accout
}



clust <- cluster(dat, centroid)

print(data.frame(Iteration = 1, 
                 Error = round(class.accuracy(dat$class, clust), 4)))
update.plot(dat, centroid, clust)

for(i in 2:10){
    centroid <- update.centroid(dat, clust)
    clust <- cluster(dat, centroid)
    print(data.frame(Iteration = i, 
                     Accuracy = round(class.accuracy(dat$class, clust), 4)))
    update.plot(dat, centroid, clust)
}


