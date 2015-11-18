#' R6 reference class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrDistance <- R6Class("cbrDistance",
                       public=list(# calculate distance matrix for new data
                         getDistanceMatrix = function(verumData) {
                           if (missing(verumData)) {
                             cat("No new data: use reference data for distance calculation!\n")
                             self$verumData <- self$learning
                           } else {
                             # validation check new data
                             self$verumData <- private$check_data(verumData, isReference=F)
                           }

                           # learn if weights are empty
                           if (class(self$Weights) != "list")
                             self$learn()

                           # Start calculation
                           start <- Sys.time()
                           cat("Start calculating distance matrix...\n")
                           # subsetting new data
                           verumData <- self$verumData[, self$learnVars]
                           self$distMat <- private$calcDist(verumData, self$learning, self$learnVars, self$Weights)
                           end <- Sys.time()
                           duration <- round(as.numeric(end - start), 2)
                           cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                         }),
                       private=list(
                         # calculate distance matrix
                         calcDist = function (newCases, learning, learnVars, Weights) {
                           trfData <- private$transform_data(newCases, learning, learnVars, Weights)
                           # drop endpoints from reference
                           # now all columns of learning and verumData are numeric,
                           # s.t. we can now apply our rcpp function
                           return(.Call("get_Distance_Matrix",
                                        trfData$newCases,
                                        trfData$learning,
                                        trfData$trafoWeights,  PACKAGE = "cbr"))
                         },
                         # calculate distance and return n nearest distance and
                         # row id of n nearest cases from reference data
                         calcNDist = function(newCases, learning, learnVars, Weights, nCases) {
                           trfData <- private$transform_data(newCases, learning, learnVars, Weights)
                           return(.Call("get_nearest_Elements",
                                        trfData$newCases,
                                        trfData$learning,
                                        trfData$trafoWeights,
                                        nCases,  PACKAGE = "cbr"))
                         },
                         transform_data = function(newCases, learning, learnVars, Weights) {
                           # data preparation:
                           # we transform all factor to their corresponding
                           # weights and set weight equal to 1 for factor
                           # variables
                           nVars <- length(learnVars)
                           trafoWeights <- rep(0, nVars)
                           for (j in 1:nVars) {
                             if (is.factor(learning[, learnVars[j]])) {
                               newCases[, learnVars[j]] <- Weights[[learnVars[j]]][newCases[, learnVars[j]]]
                               learning[, learnVars[j]] <- Weights[[learnVars[j]]][learning[, learnVars[j]]]
                               trafoWeights[j] <- 1
                             } else { # else keep weights
                               trafoWeights[j] <- Weights[[learnVars[j]]]
                             }
                           }
                           names(trafoWeights) <- NULL
                           return(list(newCases     = unname(as.matrix(newCases[,learnVars])),
                                       learning      = unname(as.matrix(learning[,learnVars])),
                                       trafoWeights = trafoWeights))
                         })
)
