#' RandomForest Proximity
#' 
#' This class uses the proximity matrix of the random survival forest algorithm 
#' as a similarity matrix (sqrt(1 - proximity matrix)) of learning and verum 
#' cases. By default all cases with at least one missing values are dropped 
#' from learning, calculating the distance matrix, and searching for similar
#' cases. 
#'
#' @param learning: data set for learning the RF model
#' @param queryData: Verum data set. For each case in the verum data, we are 
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
                   distMat    = NULL,
                   orderMat   = NULL,
                   simCases   = NULL,
                   impData    = NULL,
                   impInd     = NULL,
                   distMethod = "proximity",
                   learn=function(nCores, ntree, mtry, splitrule, ntime, nsplit, verbose, distMethod) {
                     # split rule
                     if (missing(splitrule)) {
                       splitrule <- "logrank"
                     }
                     if (!splitrule %in% c("logrank", "logrankscore"))
                       stop("splitrule should be: logrank or logrankscore")
                     
                     # verbose
                     if (missing(verbose)) {
                       verbose <- FALSE
                     }
                     
                     # tree
                     if (missing(ntree)) {
                       ntree <- 300
                     }
                     
                     # mtry
                     if (missing(mtry)) {
                       mtry <- length(self$learnVars)
                     }
                     
                     # ntime
                     if (missing(ntime)) {
                       ntime <- NULL
                     }
                     
                     # nsplit
                     if (missing(nsplit)) {
                       nsplit <- 0
                     }
                     
                     # distance calculation
                     if (!missing(distMethod)) {
                       if (!distMethod %in% c("proximity", "deep")) {
                         stop("distmethod should be: proximity or deep")
                       }
                       self$distMethod <- distMethod
                     }
                     
                     # number of cores for calculation
                     if (missing(nCores)) {
                       options(rf.cores=parallel::detectCores() - 1, mc.cores=parallel::detectCores() - 1)
                     } else {
                       nCores <- as.integer(nCores)
                       if (nCores >= parallel::detectCores()) {
                         nCores <- parallel::detectCores() - 1
                       }
                       options(rf.cores=nCores, mc.cores=nCores)
                     }
                     
                     # Timing
                     start <- Sys.time()
                     cat("Start learning...\n")
                     
                     # new data available?
                     variables <- c(self$endPoint, self$learnVars)
                     if (self$refEQNew) {
                       learnData <- self$learning[, variables]
                     } else {
                       learnData <- rbind(self$learning[, variables], self$queryData[, variables])
                     }
                     
                     # impute
                     if (self$impute) {
                       impute <- "na.impute"
                     } else {
                       impute <- "na.omit"
                     }
                     
                     # Learning
                     formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                     rsf <- randomForestSRC::rfsrc(formel,
                                                   data       = learnData,
                                                   ntree      = ntree,
                                                   mtry       = mtry,
                                                   splitrule  = splitrule,
                                                   proximity  = "all",
                                                   na.action  = impute,
                                                   importance = "none",
                                                   forest     = T,
                                                   do.trace   = verbose)
                     plot(rsf)
                     
                     # if imputation is activated, then save it
                     if (self$impute) {
                       self$impData <- rsf$imputed.data
                       self$impInd <- rsf$imputed.indv
                     } else {
                       self$impData <- NULL
                       self$impInd <- NULL
                     }
                     # get distance matrix: rsf$proximity has dimension n x n.
                     # n = nRef + nNew
                     # Dimension distance matrix:
                     # rows: reference cases
                     # columns: new cases
                     cat("Start distance calculation...\n")
                     if (self$distMethod == "proximity") {
                       if (self$refEQNew) {
                         self$distMat <- sqrt(1 - rsf$proximity)
                       } else {
                         nRef <- nrow(self$learning)
                         self$distMat <- sqrt(1 - rsf$proximity[1:nRef, (nRef + 1):ncol(rsf$proximity)])
                       }
                     } else if (self$distMethod == "depth") {
                       
                     }
                     end <- Sys.time()
                     duration <- round(as.numeric(end - start), 2)
                     cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                   },
                   calc_distance_matrix = function() {
                     if (is.null(self$distMat)) {
                       self$learn()
                     }
                   },
                   # get verum data, if there are missing values, return 
                   # imputed data
                   get_query_data = function () {
                     n <- nrow(self$learning)
                     idMissing <- self$impInd[self$impInd > n]
                     variables <- c(self$endPoint, self$learnVars)
                     if (length(idMissing) == 0) {
                       return(self$queryData)
                     } else {
                       idMissing <- idMissing - n
                       queryData <- self$queryData
                       queryData[idMissing, variables] <- self$impData[self$impInd > n, ]
                       return(queryData)
                     }
                   },
                   # get learning data, if it is imputed return imputed data
                   get_data = function () {
                     n <- nrow(self$learning)
                     idMissing <- self$impInd[self$impInd <= n]
                     variables <- c(self$endPoint, self$learnVars)
                     if (length(idMissing) == 0) {
                       return(self$learning)
                     } else {
                       learning <- self$learning
                       learning[idMissing, variables] <- self$impData[self$impInd <= n, ]
                       return(learning)
                     }
                   },
                   # calculate similar cases
                   calc_similar_cases = function(nCases) {
                     if (self$refEQNew) {
                       stop("no new data!")
                     }
                     start <- Sys.time()
                     cat("Start calculating similar cases...\n")
                     # learn if weights are empty
                     if (is.null(self$distMat)) {
                       self$learn()
                     }
                     # check nCases input
                     if (missing(nCases))
                       nCases <- 1
                     if (!is.numeric(nCases))
                       stop("nCases must be numeric!")
                     if (nCases <= 0)
                       stop("nCases must be positive integer value!")
                     # catch floating numbers
                     nCases <- as.integer(nCases)
                     #create new object & calculate similar cases
                     sc <- simCases$new(distMat=self$distMat)
                     sc$calc_similar_cases(queryData=self$queryData, learning=self$learning, nCases=nCases)
                     self$orderMat <- sc$order
                     self$simCases <- sc$similarCases
                     end <- Sys.time()
                     duration <- round(as.numeric(end - start), 2)
                     cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                   },
                   validate_model = function(plot=T) {
                     if (is.null(nrow(self$simCases)))
                       stop("no similar cases")
                     valSC <- cbrValidate$new()
                     return(valSC$validate(self$queryData, self$simCases, self$learnVars, plot))
                   }
                 ))
