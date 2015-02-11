
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
    plot(dm[,-5], col = clust, pch = 16, cex = size,)
}


plot.single <- function(dat, centroid, clust = NULL){
    k <- dim(centroid)[1]
    m <- dim(dat)[1]
    centroid$class <- "centroid"
    dm <- rbind(centroid, dat)
    clust <- c(1:k, clust)
    size <- c(rep(2,k), rep(1,m))
    plot(dm$petal.length, dm$petal.width, col = clust, pch = 16, cex = size,
         xlab = "Petal Length", ylab = "Petal Width")
    legend("bottomright", levels(dat$class), pch = 16, col = palette()[1:3])
}

# assign each observation to a cluster based on distance to centroid
cluster <- function(dat, centroid){
    k <- dim(centroid)[1]
    m <- dim(dat)[1]
    dmat <- data.frame(matrix(rep(0, k*m),m,k)) #distance matrix
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
    c(accout, perm[which.max(acc),])
}