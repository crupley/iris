
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


plot.iris <- function(dat, centroid = NULL, clust = NULL){
    # functions in one of two ways:
    #  1. plot.iris(dat) plots data colored by species
    #  2. plot.iris(dat, centroid, cluster) plots data with centroids
    #       colored by assigned cluster
    m <- dim(dat)[1]
    if(!is.null(centroid)){
        k <- dim(centroid)[1]
        centroid$class <- factor(1:3)
        dm <- rbind(centroid, dat)
        size <- c(rep(2,k), rep(1,m))
    } else {
        size <- 1
        dm <- dat
    }
    if(!is.null(clust)){
        clust <- c(1:k, clust)
        class <- factor(paste0("Cluster #",clust))
    } else {
        class <- dm$class
    }
    
    plot(dm$petal.length, dm$petal.width, col = class, pch = 16, cex = size,
         xlab = "Petal Length", ylab = "Petal Width")
    legend("bottomright", levels(class), pch = 16, col = palette()[1:3])
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
    acc <- 9e99
    for(i in 1:dim(perm)[1]){
        acc[i] <- sum(unclass(f1) == 
                          unclass(factor(f2, levels = perm[i,])))/length(f1)
    }
    accout <- max(acc)
    c(accout, perm[which.max(acc),])
}