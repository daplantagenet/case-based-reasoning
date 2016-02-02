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

sourceCpp("getDistCPP.cpp")
nTree <- 100
rfobj <- rfsrc(Surv(time, status) ~ age, 
               data       = veteran, 
               ntree      = nTree, 
               membership = T,
               proximity  = T, 
               forest     = T)
rfobj->v.obj

treeAge <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == 1, ]
DistTreeCPP <- getDistCPP(treeAge)
names(DistTreeCPP)[3] <- "tree_1"
system.time(
for (i in 2:nTree) {
  treeAge <- v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == i, ]
  tmp <- getDistCPP(treeAge)
  names(tmp)[3] <- paste0("tree_", i)
  DistTreeCPP <- full_join(DistTreeCPP, tmp, by=c("x", "y"))
})
DistTreeCPP <- as.data.table(DistTreeCPP)
setkeyv(DistTreeCPP, cols = c("x", "y"))

names <- paste0("tree_", 1:nTree)
DistTreeCPP[, c("sum", "n") := NULL]
DistTreeCPP[, sum := sum(2/exp(.SD), na.rm=T), by = c("x", "y"), .SDcols = names]
DistTreeCPP[, n := nTree - sum(is.na(.SD)), by = c("x", "y"), .SDcols = names]
DistTreeCPP$dist <- DistTreeCPP$sum / DistTreeCPP$n
setkey(DistTreeCPP, dist)
DistTreeCPP
table(DistTreeCPP$n)

system.time(
  for (i in 1:100) {
    treeAge <- v.obj $forest$nativeArray[v.obj$forest$nativeArray$treeID == i, ]
    DistTreeCPP<-getDist(treeAge)
})

DistTree <- getDist(treeAge)
identical(DistTree,DistTreeCPP)

