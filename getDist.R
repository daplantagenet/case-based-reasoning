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
  # TODO: allocate df 
  l <- (length(mynv)^2 - length(mynv)) / 2
  #myol <- data.frame(x=rep(0L, l), y=rep(0L, l), dist=rep(0L, l))
  k <- 1
  for (i in 1:(length(mynv) - 1)) {
    for (j in (i + 1):length(mynv)) {
      id <- which.min(myil[[i]][2:length(myil[[i]])] %in% myil[[j]][2:length(myil[[j]])])
      x <- nodeID[i]
      x <- nodeID[j]
      x <- myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2
      k <- k + 1
    }
  }
  # df -> dt and set keys for fast lookup
#   myol <- as.data.table(myol)
#   setkeyv(myol, c("x", "y")) 
  #return(myol)
}

get_list <-  function(x) {
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
  
  return(myil)
}
