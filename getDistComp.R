library("Rcpp")
getDist <- function (x) {
  
  myv <- is.na(x$contPT)
  mynv <- which(myv)
  nodeID <- x$nodeID[mynv]
  
  g <- c(0, 0)
  myil <- list()
  for (i in 2:length(myv)) {
    if (myv[i - 1]) {
      myil <- append(myil, list(g))
      g <- c(g[length(g)], g[2:(length(g) - 1)])
    } else {
      g <- c(g[1] + 1, g[2:length(g)], g[1] + 1)
    }
  }
  myil <- append(myil, list(g))
  
  
  # get pairwise distance; save nodeID as RF return in prediction nodeID
  # length: (length(mynv)^2 - length(mynv)) / 2; 
  l <- (length(mynv)^2 - length(mynv)) / 2
  myDist <- data.frame(x=rep(0L, l), y=rep(0L, l), dist=rep(0, l))
  k <- 1
  for (i in 1:(length(mynv) - 1)) {
    for (j in (i + 1):length(mynv)) {
      id <- which.min(myil[[i]][2:length(myil[[i]])] %in% myil[[j]][2:length(myil[[j]])])
      myDist$x[k] <- nodeID[i]
      myDist$y[k] <- nodeID[j]
      myDist$dist[k] <- myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2    
      k <- k + 1
    }
  }
  return(myDist)
}

# Example Usage ----
library(randomForestSRC)
library(data.table)
library(dplyr)
data(veteran, package = "randomForestSRC")

# source cpp code
sourceCpp("getDistCPP.cpp")
nTree <- 10

# fit random forrest
rfobj <- rfsrc(Surv(time, status) ~ .,
             data       = veteran,
             ntree      = nTree,
             membership = T,
             proximity  = T,
             forest     = T)
rfobj -> v.obj


treeAgeT <- v.obj$forest$nativeArray
prf <- predict(rfobj)
membership<-prf$membership

# create csv files for tests
write.csv(treeAgeT, file="treeAge.csv")
write.csv(membership, file="membership.csv")

# treeAge from file for tests
treeAgeT <- read.csv("treeAge.csv")
treeAgeT <-treeAgeT [-1]

# membership from file for tests
membership <- read.csv("membership.csv")
membership <-membership [-1]


#treeAgeT <- v.obj$forest$nativeArray
nTree <- 10

# get the number of knotes for each end node and for each tree and save it in
# a data.table; 
# I have replaced following loop by your C++-Code.
t <- Sys.time()
treeAge <- treeAgeT[treeAgeT$treeID == 1, ]
DistTreeCPP <- getDistCPP(treeAge)
names(DistTreeCPP)[3] <- "tree_1"
for (i in 2:nTree) {
  treeAge <- treeAgeT[treeAgeT $treeID== i, ]
  tmp <- getDistCPP(treeAge)
  names(tmp)[3] <- paste0("tree_", i)
  DistTreeCPP <- full_join(DistTreeCPP, tmp, by=c("x", "y"))
}
DistTreeCPP <- as.data.table(DistTreeCPP)
setkeyv(DistTreeCPP, cols = c("x", "y"))
Sys.time() - t

n <- nrow(membership)
el <- (n^2 - n) / 2
d <- rep(0, el)
d <- matrix(0, n, n)
w <- .1
t<-Sys.time()
l <- 1
for (i in 1:n) {
  for (j in (i + 1):n) {
    for (k in 1:ncol(membership)) {
      x1 <- membership[i, k]
      x2 <- membership[j, k]
      if (!is.na(x1) && !is.na(x2)) {
        dk <- 0
        if (x1 != x2) {
          xmin <- min(x1, x2)
          xmax <- max(x1, x2)
          dk <- as.numeric(DistTreeCPP[J(xmin, xmax), k + 2, with=F])
        }
        if (!is.na(dk))
          d[i, j] <- d[i, j] + 1 / exp(w * dk)
          #d[l] <- d[l] + 1 / exp(w * dk)
      }
    }
    l <- l + 1
  }
}
Sys.time() - t
# d <- 1 - d / ncol(membership)
d





# source cpp code for the creating all trees and for the counting the distance between 2 nodes 
sourceCpp("getMapTrees.cpp")

t<-Sys.time()
DistTreeAll<-getDistForTreesCPP(treeAgeT, nTree)
Sys.time()-t

DistTreeAll[DistTreeAll == -1] <- NA

# compare the dataframes for all trees
all.equal(DistTreeAll, DistTreeCPP, check.attributes=F)


DistTreeCPP[J(18, 24), 3, with=F]
getGijt(treeAgeT, nTree, 18, 24, 3)

getGijt(treeAgeT, nTree, 2, 29, 4)
getDXiXj(treeAgeT, nTree, 2, 23)
getDXiXj(treeAgeT, nTree, 2, 29)
getDXiXj(treeAgeT, nTree, 2, 29, 6)
getDXiXj(treeAgeT, nTree, 6, 6, 0)
getGijt(treeAgeT, nTree, 2, 23, 4)
getDXiXj(treeAgeT, nTree, 2, 23,0)
getDXiXj(treeAgeT, nTree, 1, 29)
getDXiXj(treeAgeT, nTree, 1, 29,0)


# Description for Simon with my membership matrix: This is the most critical performance part of the whole 
# calculations:
# predict; in this case, we also can use v.obj$membership
# prf <- predict(rfobj)
# the membership matrix (n x nTree) with n = cases contains the end node 
# membership of each case. Example my output:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] ...
# [1,]   24   24    24   25   29   26   23   27    2     4    29    25    25  
# [2,]   18   16    25   21   3    16   19   17    7    18    26    17    20 
# [3,]   4     5     5    6   10    1    5    2    5     7     7     2     7    
# 
# Let's have a look at tree 1. 
# Case 1 is in end node 24 and case 2 in end node 
# 18 the number of knotes between this nodes can be extracted from DistTreeCPP 
# by DistTreeCPP[J(18, 24), 3, with=F]. The smaller number has to be always 
# in the first field of J(., .) as DistTreeCPP is ordered by x then by y and the
# (x, y) pairs are unique. Say you get there the value 4. On this value apply 
# 1 / exp(w * g_ijt) = 1 / exp(w * 4), where w is a parameter that can be
# set as parameter (default: w = 2). Now, we have two possibilities
# 1. loop over trees or loop over cases. As this algorithm should be applied 
# on large data sets and n >> nTree, I would suggest to parallelize over cases 
# with (n^2 - n) / 2 final fields or iterations). You may have to research a good 
# matrix representation in the matrix package. 



# compare to R-Code
system.time(
  for (i in 1:nTree) {
    treeAge <- treeAgeT[treeAgeT$treeID == i, ]
   # DistTreeCPP<-getDist(treeAge)
})



