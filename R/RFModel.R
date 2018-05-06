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
RFModel <- R6Class("RFModel",
                   inherit = cbrData,
                   public=list(
                     rangerObj  = NULL,
                     methodArgs = NULL,
                     distMat    = NULL,
                     orderMat   = NULL,
                     simCases   = NULL,
                     distMethod = "depth",
                     print = function() {
                       cat("Case-Based-Reasoning with RandomForests\n")
                       cat("---------------------------------------\n")
                       cat("Endpoints : ", paste(self$endPoint, collapse = ", "))
                       cat("Variables : ", paste(self$terms, collapse = ", "))
                       cat("Trained   : ", ifelse(is.null(self$rangerObj), FALSE, TRUE))
                     },
                     initialize = function(formula, ntree = 500, mtry = NULL, splitrule="maxstat", minprop=.35, ...) {
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
                         minprop = minprop
                       )
                       self$methodArgs <- args
                     },
                     fit = function(dtData) {
                       dtData %>%
                         dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                       dtData <- private$check_data(dtData)
                       
                       # Timing
                       start <- Sys.time()
                       cat("Start learning...\n")
                       self$data %>%
                         dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                       
                       # Learning
                       self$rangerObj <- ranger::ranger(formula      = self$formula,
                                                        data         = dtData,
                                                        num.trees    = self$methodArgs$ntree,
                                                        mtry         = self$methodArgs$mtry,
                                                        splitrule    = self$methodArgs$splitrule, 
                                                        num.threads  = NULL, # self$methodArgs$nCores,
                                                        write.forest = T,
                                                        verbose      = T)
                       end <- Sys.time()
                       
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                     },
                     transform = function() {
                       
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
