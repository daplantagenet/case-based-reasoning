#' Case Based Reasoning distance calculation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrDistance <- R6Class("cbrDistance",
                       public=list(# calculate distance matrix for new data
                         getFullDistanceMatrix = function(newData) {
                           if (missing(newData)) {
                             cat("No new data: use reference data for distance calculation!\n")
                             self$newData <- self$refData
                           } else {
                             # validation check new data
                             self$newData <- private$check_data(newData, isReference=F)
                           }

                           # learn if weights are empty
                           if (class(self$Weights) != "list")
                             self$learn()

                           # Start calculation
                           start <- Sys.time()
                           cat("Start calculating distance matrix...\n")
                           # subsetting new data
                           newData <- self$newData[, self$learnVars]
                           self$distMat <- private$calcDist(newData, self$refData, self$learnVars, self$Weights)
                           end <- Sys.time()
                           duration <- round(as.numeric(end - start), 2)
                           cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                         }),
                       private=list(
                         # calculate distance matrix
                         calcDist = function (newCases, refData, learnVars, Weights) {
                           trfData <- private$transform_data(newCases, refData, learnVars, Weights)
                           # drop endpoints from reference
                           # now all columns of refData and newData are numeric,
                           # s.t. we can now apply our rcpp function
                           return(.Call("cbr_get_Distance_Matrix",
                                        trfData$newCases,
                                        trfData$refData,
                                        trfData$trafoWeights,  PACKAGE = "cbr"))
                         },
                         # calculate distance and return n nearest distance and
                         # row id of n nearest cases from reference data
                         calcNDist = function(newCases, refData, learnVars, Weights, nCases) {
                           trfData <- private$transform_data(newCases, refData, learnVars, Weights)
                           return(.Call("get_nearest_Elements",
                                        trfData$newCases,
                                        trfData$refData,
                                        trfData$trafoWeights,
                                        nCases,  PACKAGE = "cbr"))
                         },
                         transform_data = function(newCases, refData, learnVars, Weights) {
                           # data preparation:
                           # we transform all factor to their corresponding
                           # weights and set weight equal to 1 for factor
                           # variables
                           nVars <- length(learnVars)
                           trafoWeights <- rep(0, nVars)
                           for (j in 1:nVars) {
                             if (is.factor(refData[, learnVars[j]])) {
                               newCases[, learnVars[j]] <- Weights[[learnVars[j]]][newCases[, learnVars[j]]]
                               refData[, learnVars[j]] <- Weights[[learnVars[j]]][refData[, learnVars[j]]]
                               trafoWeights[j] <- 1
                             } else { # else keep weights
                               trafoWeights[j] <- Weights[[learnVars[j]]]
                             }
                           }
                           names(trafoWeights) <- NULL
                           return(list(newCases     = unname(as.matrix(newCases[,learnVars])),
                                       refData      = unname(as.matrix(refData[,learnVars])),
                                       trafoWeights = trafoWeights))
                         })
)