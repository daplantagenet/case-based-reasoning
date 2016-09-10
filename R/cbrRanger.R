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
#' rangerTerminalNodeIds(rf, iris[, -5])
#' }
#' 
#' @export
rangerTerminalNodeIds <- function(rf, x) {
  x <- as.matrix(x)
  res=sapply(1:rf$num.trees, function(tree) {
    cbr:::terminalNodeIDs(x = x, 
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
#' rangerProximityMatrix(rf, iris[, -5])
#' }
#' 
#' @export
rangerProximityMatrix <- function(rf, x) {
  x <- as.matrix(x)
  nodes <- rangerTerminalNodeIds(x, rf)
  d <- proximityMatrix(nodes)
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
