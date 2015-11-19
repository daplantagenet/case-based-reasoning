#' R6 reference class for getting the similar cases
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
simCases <- R6Class("simCases",
                    public=list(
                      distMat      = NA,
                      distOrder    = NA,
                      similarCases = NA,
                      initialize = function(distMat) {
                        if (missing(distMat)) {
                          self$distMat <- NA
                        } else {
                          self$distMat <- distMat
                        }
                      },
                      # calculate distance matrix for new data
                      getDistanceMatrix = function(verumData, learning, learnVars, Weights) {
                        # Start calculation
                        if (is.na(self$distMat)) {
                          return(private$calcDist(verumData, learning, learnVars, Weights))
                        } else {
                          self$distMat
                        }
                      },
                      # get similar cases from reference data
                      getSimilarCases = function(verumData, learning, nCases) {
                        if (is.na(self$distMat)) {
                          stop("Need distance Matrix!")
                        }
                        
                        # fast ordering of similar cases
                        ordDist <- private$getOrder(nCases)
                        # get most similar cases
                        similarCases <- do.call(rbind, apply(ordDist, 1,
                                                             function(x, data=learning) {
                                                               data[x, ]
                                                             }
                        )
                        )
                        # mark similar cases: 1:n ids
                        similarCases$caseId <- rep(1:nrow(verumData), each=nCases)
                        self$distOrder <- ordDist
                        self$similarCases <- similarCases
                      }),
                    private=list(
                      # calculate distance matrix; deprecated
                      calcDist = function (newCases, learning, learnVars, Weights) {
                        trfData <- private$transform_data(newCases, learning, learnVars, Weights)
                        # drop endpoints from reference
                        # now all columns of learning and verumData are numeric,
                        # s.t. we can now apply our rcpp function
                        return(.Call("cbr_get_Distance_Matrix",
                                     trfData$newCases,
                                     trfData$learning,
                                     trfData$trafoWeights, PACKAGE = "cbr"))
                      },
                      # calculate distance and return n nearest distance and
                      # row id of n nearest cases from reference data; deprecated
                      calcNDist = function(newCases, learning, learnVars, Weights, nCases) {
                        trfData <- private$transform_data(newCases, learning, learnVars, Weights)
                        return(.Call("cbr_get_nearest_Elements",
                                     trfData$newCases,
                                     trfData$learning,
                                     trfData$trafoWeights,
                                     nCases, PACKAGE = "cbr"))
                      },
                      getOrder = function(nCases) {
                        return(.Call("cbr_fast_Matrix_Order",
                                     self$distMat,
                                     nCases, PACKAGE = "cbr"))
                      },
                      transform_data = function(newCases, learning, learnVars, Weights) {
                        # data preparation:
                        # we transform all factors to their corresponding
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
                        return(list(newCases     = unname(as.matrix(newCases[, learnVars])),
                                    learning     = unname(as.matrix(learning[, learnVars])),
                                    trafoWeights = trafoWeights))
                      }
                    ))
