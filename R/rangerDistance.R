#' Get terminal node IDs of observations of an ranger object
#'
#' @param x a new dataset
#' @param rf \code{ranger} object
#' 
#' @return Matrix with terminal node IDs for all observations in x (rows) and
#'         trees (columns)
#'         
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' terminalNodeIdsRanger(iris[, -5], rf)
#' }
#' 
#' @export
terminalNodeIdsRanger <- function(x, rf) {
  x <- as.matrix(x)
  res=sapply(1:rf$num.trees, function(tree) {
    terminalNodeIDRanger(x = x, 
                         childNodes1 = rf$forest$child.nodeIDs[[tree]][[1]], 
                         childNodes2 = rf$forest$child.nodeIDs[[tree]][[2]], 
                         splitValues = as.double(rf$forest$split.values[[tree]]),
                         splitVarIds = rf$forest$split.varIDs[[tree]])
  }, simplify = F)
  res <- do.call(cbind, res)
  as.matrix(res)
}


#' Get proximity matrix of an ranger object
#'
#' @param x a new dataset
#' @param y a second new dataset (Default: NULL)
#' @param rf \code{ranger} object
#' 
#' @return a \code{dist} or a matrix object with pairwise proximity of 
#' observations in x vs y (if not null)
#'      
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' proximityMatrixRanger(x = iris[, -5], rf = rf)
#' 
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf <- ranger(Species ~ ., data = iris[learn, ], num.trees = 5, write.forest = TRUE)
#' proximityMatrixRanger(x = iris[learn, -5], y = iris[test, -5], rf = rf)
#' }
#' 
#' @export
proximityMatrixRanger <- function(x, y = NULL, rf) {
  x %>% 
    as.matrix() %>% 
    terminalNodeIdsRanger(rf) -> xNodes
  if (is.null(y)) {
    d <- proximityMatrixRangerCPP(xNodes)
    n <- nrow(x)
    # convert to dist object
    asDistObject(d, n, "RFProximity")
  } else {
    y %>% 
      as.matrix() %>% 
      terminalNodeIdsRanger(rf) -> yNodes
    proximityMatrixRangerCPPNM(xNodes, yNodes)
  }
}


#' Get depth distance matrix
#' 
#' @param x a new dataset
#' @param y a new dataset
#' @param rf \code{ranger} object
#' 
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' depthMatrixRanger(x=iris[, -5], rf=rf)
#' }
#' 
#' @export
depthMatrixRanger <- function(x, y=NULL, rf) {
  x %>% 
    as.matrix() %>% 
    terminalNodeIdsRanger(rf) -> xNodes
  rf %>% 
    rangerTreeAsMat() -> rfTrees
  if (is.null(y)) {
    d <- depthMatrixRangerCPP(xNodes, rfTrees) 
    n <- nrow(x)
    # convert to dist object
    asDistObject(d, n, "RFDepth")
  } else {
    y %>% 
      as.matrix() %>% 
      terminalNodeIdsRanger(rf) -> yNodes
    depthMatrixRangerCPPXY(xNodes, yNodes, rfTrees)
  }
}


#' Terminal node distance for each tree and terminal
#' 
#' first two columns are terminal node IDs; If an ID pair do not appear in a 
#' tree -1 is inserted
#'   
#' @param rf \code{ranger} object
#' 
#' @return a \code{matrix} object with pairwise terminal node edge length
#'    
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' terminalNodeDistance(rf)
#' }
#' 
#' @export
terminalNodeDistance <- function(rf) {
  rangerTreeAsMat(rf) %>% 
    terminalNodeDistanceCPP()
}


#' Transform trees of a \code{ranger}-object to a matrix
#' 
#' @param rf \code{ranger} object
#' 
#' @return a \code{matrix} object with 
#' Column 1: tree ID
#' Column 2: node ID
#' Column 3: child node ID 1
#' Column 4: child node ID 2
#'
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' rangerTreeAsMat(rf)
#' }
#' @export
rangerTreeAsMat <- function(rf) {
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