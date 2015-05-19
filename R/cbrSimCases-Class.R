#' Case Based Reasoning Get Similar Cases
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
simCases <- R6Class("simCases",
                    public=list(
                      distMat      = NA,
                      distOrder    = NA,
                      similarCases = NA,
                      method       = NA,
                      initialize = function(distMat, method="cox") {
                        # for RF
                        if (!missing(distMat)) {
                          self$distMat <- distMat
                        }

                        if (method == "rfProxy" & missing(distMat)) {
                          stop("For Random Forest a distance matrix is needed!")
                        }
                        self$method <- method
                      },
                      # calculate distance matrix for new data
                      getFullDistanceMatrix = function(newData, refData, learnVars, Weights) {
                        # Start calculation
                        return(private$calcDist(newData, refData, learnVars, Weights))
                      },
                      # get similar cases from reference data
                      getSimilarCases = function(newData, refData, learnVars, Weights, nCases) {
                        if (self$method == "cox") {
                          # calculate distance and order of cases based on distance calculation
                          ordDist <- private$calcNDist(newData, refData, learnVars, Weights, nCases)
                          # get most similar cases
                          similarCases <- do.call(rbind, apply(ordDist$order, 2,
                                                               function(x, data=refData) {
                                                                 data[x, ]
                                                               }
                          )
                          )
                          # mark similar cases: 1:n ids
                          similarCases$caseId <- rep(1:nrow(newData), each=nCases)
                          # get distances
                          # distList <- apply(ordDist$distance, 2, list)
                          # similarCases$distance <- unlist(lapply(distList, function(x, n=nCases) {or <- order(x[[1]]);x[[1]][or[1:n]]}))
                          self$distMat <- ordDist$distance
                          self$distOrder <- ordDist$order
                          self$similarCases <- similarCases
                        } else if (self$method == "rfProxy") {
                          # fast ordering of similar cases
                          ordDist <- private$getOrder(nCases)
                          # get most similar cases
                          similarCases <- do.call(rbind, apply(ordDist, 1,
                                                               function(x, data=refData) {
                                                                 data[x, ]
                                                               }
                          )
                          )
                          # mark similar cases: 1:n ids
                          similarCases$caseId <- rep(1:nrow(newData), each=nCases)
                          self$distOrder <- ordDist
                          self$similarCases <- similarCases
                        }
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
                                     trfData$trafoWeights, PACKAGE = "cbr"))
                      },
                      # calculate distance and return n nearest distance and
                      # row id of n nearest cases from reference data
                      calcNDist = function(newCases, refData, learnVars, Weights, nCases) {
                        trfData <- private$transform_data(newCases, refData, learnVars, Weights)
                        return(.Call("cbr_get_nearest_Elements",
                                     trfData$newCases,
                                     trfData$refData,
                                     trfData$trafoWeights,
                                     nCases, PACKAGE = "cbr"))
                      },
                      getOrder = function(nCases) {
                        return(.Call("fast_Matrix_Order",
                                     self$distMat,
                                     nCases, PACKAGE = "cbr"))
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
                        return(list(newCases     = unname(as.matrix(newCases[, learnVars])),
                                    refData      = unname(as.matrix(refData[, learnVars])),
                                    trafoWeights = trafoWeights))
                      }
                    ))