#' Distance calculation
#'
#' @param rf \code{ranger} object
#' @param x a new dataset
#' 
#' @return a \code{dist} or \code{matrix} object
#'         
#' @examples
#' \dontrun{
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' terminalNodeIdsRanger(iris[, -5], rf)
#' }
#' 
#' @export
distance <- function(x, y=NULL, w=NULL, method="euclidian", p=2) {
  
}
