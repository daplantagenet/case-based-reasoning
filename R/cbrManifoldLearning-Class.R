#' R6 reference class for getting the similar cases
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
manifoldLearning <- R6Class("manifoldLearning",
                            public=list(method = NA,
                                        initialize = function(method="tsne") {
                                          
                                        })
                            )
