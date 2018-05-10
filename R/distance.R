#' Distance calculation based on RandomForest Proximity or Depth
#' 
#' @param x a data.frame
#' @param y a second data.frame
#' @param rfObject \code{ranger} object
#' @param method distance calculation method
#' 
#' @return a \code{dist} or a matrix object with pairwise distance of 
#' observations in x vs y (if not null)
#' 
#' @examples
#' \dontrun{
#' library(ranger)
#' # get proximity pairwise distances
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' distanceRandomForest(x = iris[, -5], rfObject = rf.fit, method = "Proximity")
#' 
#' # depth distance for train versus test subset
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf <- ranger(Species ~ ., data = iris[learn, ], num.trees = 5, write.forest = TRUE)
#' distanceRandomForest(x = iris[learn, -5], y = iris[test, -5], rfObject = rf.fit, method = "Depth")
#' }
#' 
#' @export
distanceRandomForest <- function(x, y = NULL, rfObject, method = "Proximity", threads = NULL) {
  method <- match.arg(method, c("Proximity", "Depth"))
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  # Data Preparation
  x %>% 
    as.matrix() %>% 
    terminalNodeIDs(rfObject) -> xNodes
  if (!is.null(y)) {
    testthat::expect_equal(colnames(x), colnames(y))
    y %>% 
      as.matrix() %>% 
      terminalNodeIDs(rfObject) -> yNodes
  } else {
    yNodes <- xNodes
  }
  input <- list(xNodes, yNodes)
  
  # Arguments
  arguments <- list()
  arguments["method"] <- method
  
  # Attributes
  N <- ifelse(is.list(x), length(x), nrow(x))
  attrs <- list(Size = N, Labels = names(x), Diag = diag, Upper = upper,
                method = METHODS[methodIdx], call = match.call(), class = "dist")
  
  # set number of threads
  if (!is.null(threads)) {
    RcppParallel::setThreadOptions(numThreads = threads)
  }
  
  cpp_parallelDistance(dataList = input, attrs = attrs, arguments = arguments)
}


#' @title Calculate terminal node distance for each tree and terminal
#' 
#' @description first two columns are terminal node IDs; If an ID pair do not 
#' appear in a tree -1 is inserted
#'   
#' @param rfObject \code{ranger} object
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
distanceTerminalNodes <- function(rfObject) {
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  rfObject %>% 
    forestToMatrix() %>% 
    cpp_TerminalNodeDistance()
}