#' Get terminal node IDs of observations of an ranger object
#'
#' @param rf \code{ranger} object
#' @param x a new dataset
#' 
#' @return Matrix with terminal node IDs for all observations in x (rows) and
#'         trees (columns)
#'         
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' terminalNodeIdsRanger(rf, iris[, -5])
#' }
#' 
#' @export
terminalNodeIdsRanger <- function(rf, x) {
  x <- as.matrix(x)
  res=sapply(1:rf$num.trees, function(tree) {
    cbr:::terminalNodeIDRanger(x = x, 
                               childNodes1 = rf$forest$child.nodeIDs[[tree]][[1]], 
                               childNodes2 = rf$forest$child.nodeIDs[[tree]][[2]], 
                               splitValues = as.double(rf$forest$split.values[[tree]]),
                               splitVarIds = rf$forest$split.varIDs[[tree]])
  })
  return(res)
}


#' Get proximity matrix of an ranger object
#'
#' @param rf \code{ranger} object
#' @param x a new dataset
#' 
#' @return a \code{dist} object with pairwise proximity of observations in x
#'         
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' proximityMatrixRanger(rf, iris[, -5])
#' }
#' 
#' @export
proximityMatrixRanger <- function(rf, x) {
  x <- as.matrix(x)
  nodes <- terminalNodeIdsRanger(rf, x)
  d <- cbr:::proximityMatrixRangerCPP(x = nodes, nTrees = rf$num.trees)
  n <- nrow(x)
  # convert to dist object
  structure(.Data  = d,
            Size   = n,
            Labels = 1:n,
            Diag   = F,
            Upper  = F,
            method = "rangerProximity",
            class  = "dist")
}


rangerRFtoMat <- function(rf) {
  res <- sapply(1:rf$num.trees, function(t) {
    len <- length(rf$forest$child.nodeIDs[[t]][[1]])
    data.frame(t   = rep(t, len), 
          n   = seq_len(len),
          id1 = rf$forest$child.nodeIDs[[t]][[1]],
          id2 = rf$forest$child.nodeIDs[[t]][[2]])
  }, simplify = F)
  res <- do.call(rbind, res)
  as.matrix(res)
}
