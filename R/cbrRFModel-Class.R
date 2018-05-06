#' RandomForest Proximity
#' 
#' This class uses the proximity matrix of the random survival forest algorithm 
#' as a similarity matrix (sqrt(1 - proximity matrix)) of learning and verum 
#' cases. By default all cases with at least one missing values are dropped 
#' from learning, calculating the distance matrix, and searching for similar
#' cases. 
#'
#' @param formula : formula for learning the Cox model
#' @param data    : the dataset for learning the model
#' @param queryData (optional) : Query data set. For each case in the query data,
#'  we are looking for the k (=1,…,l) similar cases in the learning data.
#'  Learning and query datasets need the same structure (variable names and scales)
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrRFModel <- R6Class("cbrRFModel",
                      inherit = cbrData,
                      public=list(
                        rangerObj  = NULL,
                        methodArgs = NULL,
                        distMat    = NULL,
                        orderMat   = NULL,
                        simCases   = NULL,
                        distMethod = "depth",
                        initialize = function(formula, ntree = 500, mtry = NULL, splitrule="maxstat", minprop=.35, save.memory=T, ...) {
                          # split rule
                          if (missing(splitrule)) {
                            splitrule <- "logrank"
                          }
                          splitrule <- match.arg(splitrule, c("logrank", "maxstat", "C"))
                          
                          super$initialize(formula)
                          args <- list(
                            ntree = ntree,
                            mtry = mtry,
                            splitrule = splitrule,
                            minprop = minprop,
                            save.memory = save.memory
                          )
                          self$methodArgs <- args
                        },
                        fit=function(dtData) {
                          
                          # Timing
                          start <- Sys.time()
                          cat("Start learning...\n")
                          # new data available?
                          variables <- c(self$endPoint, self$learnVars)
                          dtData %>%
                            dplyr::select_(.dots = all.vars(self$formula)) -> dtData
                          
                          # Learning
                          self$rangerObj <- ranger::ranger(formula      = self$formula,
                                                           data         = dtData,
                                                           num.trees    = ntree,
                                                           mtry         = mtry,
                                                           splitrule    = splitrule, 
                                                           num.threads  = 6,
                                                           write.forest = T,
                                                           save.memory  = save.memory)
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                        },
                        set_dist=function(distMethod = "depth") {
                          distMethod <- match.arg(distMethod, c("proximity", "depth"))
                          self$distMethod <- distMethod
                        }
                      ),
                      private = list(
                        get_distance_matrix = function(distMethod = "depth") {
                          # distance calculation
                          if (!self$distMethod %in% c("proximity", "depth")) {
                            stop("Error: distMethod should be: proximity or depth.")
                          }
                          
                          if (is.null(self$rangerObj)) {
                            self$learn()
                          }
                          
                          if (self$distMethod == "proximity") {
                            self$distMat <- proximityMatrixRanger(x  = private$to_int(self$data),
                                                                  y  = private$to_int(self$queryData), 
                                                                  rf = self$rangerObj)
                            # transform to distance
                            self$distMat <- sqrt(1 - self$distMat)
                          } else if (self$distMethod == "depth") {
                            self$distMat <- depthMatrixRanger(x  = private$to_int(self$data),
                                                              y  = private$to_int(self$queryData), 
                                                              rf = self$rangerObj)
                          }
                        }
                      )
)
