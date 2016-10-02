#' RandomForest Proximity
#' 
#' This class uses the proximity matrix of the random survival forest algorithm 
#' as a similarity matrix (sqrt(1 - proximity matrix)) of learning and verum 
#' cases. By default all cases with at least one missing values are dropped 
#' from learning, calculating the distance matrix, and searching for similar
#' cases. 
#'
#' @param learning: data set for learning the RF model
#' @param queryData: Query data set. For each case in the verum data, we are 
#' looking for the k (=1,…,l) similar cases. Learning and verum data set need 
#' the same structure (variable names and scales)
#' @param learnVars (Default: all variables except endPoint): A character vector 
#' variable names. This variables are used for learning the model. Do not 
#' include time2event and event variable here. 
#' @param endPoint (Default: c("Time2Event", "Event")): A character vector of 
#' length two. The first elements contains the variable name of the time 2 event 
#' variable and the second the name of the event variable. 
#' @param impute (Default: FALSE): Missing value imputation.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrRF <- R6Class("cbrRF",
                 inherit = cbrData,
                 public=list(
                   rangerObj  = NULL,
                   distMat    = NULL,
                   orderMat   = NULL,
                   simCases   = NULL,
                   distMethod = "depth",
                   learn=function(ntree = 500, mtry = NULL, splitrule="logrank", minprop=.05, save.memory=T) {
                     # split rule
                     if (missing(splitrule)) {
                       splitrule <- "logrank"
                     }
                     if (!splitrule %in% c("logrank", "C"))
                       stop("Error: splitrule should be: logrank, C, or maxstat.")
                     
                     # Timing
                     start <- Sys.time()
                     cat("Start learning...\n")
                     
                     # new data available?
                     variables <- c(self$endPoint, self$learnVars)
                     self$data %>%
                       dplyr::select_(.dots = all.vars(self$formula)) -> dtData
                     
                     # Learning
                     self$rangerObj <- ranger::ranger(formula      = self$formula,
                                                      data         = dtData,
                                                      ntree        = ntree,
                                                      mtry         = mtry,
                                                      splitrule    = splitrule, 
                                                      write.forest = T,
                                                      save.memory  = save.memory)
                     end <- Sys.time()
                     duration <- round(as.numeric(end - start), 2)
                     cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                   },
                   set_dist=function(distMethod = "depth") {
                     # distance method
                     if (!distMethod %in% c("proximity", "depth")) {
                       stop("Error: distMethod should be: proximity or depth.")
                     }
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
                       stop("Error: Please fit model.")
                     }
                     
                     
                   }
                 )
                 )
