library("Rcpp")
library(data.table)
# Example Usage ----

# treeAge from file for tests
treeAgeT <- read.csv("treeAge.csv")
treeAgeT <-treeAgeT [-1]

# membership from file for tests
membership <- read.csv("membership.csv")
membership <-membership [-1]

# source cpp code within parallel calculation and without parallel calculation  
sourceCpp("parallelMatrixAndOther.cpp")

t<-Sys.time()
parallelMatrixDistance <- getParallelMatixDistances(treeAgeT, membership, w = .5)
Sys.time()-t
h <- hclust(as.dist(parallelMatrixDistance))
plot(h)

library(Rtsne)
p <- Rtsne(sqrt(1 - parallelMatrixDistance), is_distance = T, theta = 1)
p
plot(p$Y, col=as.factor(veteran$prior))


# create csv files for parallelMatrixDistance
write.csv(parallelMatrixDistance, file="parallelMatrixDistance.csv")

t <- Sys.time()
matrixDistance<-getMatixDistances(treeAgeT,  membership)
Sys.time() - t

# create csv files for matrixDistance
write.csv(matrixDistance, file="matrixDistance.csv")

#compare 2 results
all(parallelMatrixDistance - matrixDistance < 0.00000000000001)
