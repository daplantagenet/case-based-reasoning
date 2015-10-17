library(randomForestSRC)
data(veteran, package = "randomForestSRC")
v.obj <- rfsrc(Surv(time, status) ~ age, data = veteran, ntree = 20, membership = T,
               proximity = T, forest=T)
v.max <- max.subtree(v.obj)
id1 <- which(v.obj$inbag[1, ] > 0)
id2 <- which(v.obj$inbag[2, ] > 0)
id <- intersect(id1, id2)
v.obj$membership[1, id] - v.obj$membership[2, id]

v.obj$proximity[1, 2]

v.obj$forest$nativeArray[v.obj$forest$nativeArray$treeID == 1, ]
rf2rfz(v.obj, "test")
