library(randomForestSRC)
library(dplyr)
data(veteran, package = "randomForestSRC")

getDist_bak <- function (x) {
  nID <- x$nodeID
  tNID <- c()
  g <- c(0, 0)
  myil <- list()
  for (i in 2:length(nID)) {
    if (nID[i - 1] == nID[i]) {
      g <- c(g[1] + 1, g[2:length(g)], g[1] + 1)
    } else {
      tNID <- c(tNID, nID[i - 1])
      myil <- append(myil, list(g))
      g <- c(g[length(g)], g[2:(length(g) - 1)])
    }
  }
  tNID <- c(tNID, nID[i])
  myil <- append(myil, list(g))
  for (i in 1:(length(tNID) - 1)) {
    for (j in (i + 1):length(tNID)) {
      id <- 2
      found <- FALSE
      while (!found) {
        if ((length(myil[[i]]) <= id) || (length(myil[[j]]) <= id)) {
          found <- TRUE
        } else {
          if (myil[[i]][id + 1] != myil[[j]][id + 1]) {
            found <- TRUE
          } else {
            id <- id + 1
          }
        }
      }
#       print("=")
#       print(tNID[i])
#       print(tNID[j])
#       print(myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2)
    }
  }
}

# berechne nun für jeden Baum die Distanzmatrix ----

# 100 Bäume, Alter
v.obj <- rfsrc(formula    = Surv(time, status) ~ age, 
               data       = veteran, 
               ntree      = 100, 
               membership = T,
               proximity  = T, 
               forest     = T)
rfTrees <- v.obj$forest$nativeArray
system.time(rfTrees %>% 
  group_by(treeID) %>% 
    get_list() -> d) # ~10s

get_list()

# 100 Bäume, alle Variablen
v.obj <- rfsrc(formula    = Surv(time, status) ~ ., 
               data       = veteran, 
               ntree      = 100, 
               membership = T,
               proximity  = T, 
               forest     = T)
rfTrees <- v.obj$forest$nativeArray
system.time(rfTrees %>% 
              group_by(treeID) %>% 
              getDist()) # 26.5s

# 1000 Bäume, Alter
v.obj <- rfsrc(formula    = Surv(time, status) ~ age, 
               data       = veteran, 
               ntree      = 1000, 
               membership = T,
               proximity  = T, 
               forest     = T)
rfTrees <- v.obj$forest$nativeArray
system.time(rfTrees %>% 
              group_by(treeID) %>% 
              getDist()) # ~10s
