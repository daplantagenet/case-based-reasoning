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
                          # diag(self$distMat) <- Inf
                        }
                      },
                      # calculate distance matrix for new data
                      get_distance_matrix = function(queryData, learning, learnVars, weights) {
                        # Start calculation
                        if (class(self$distMat) != "matrix") {
                          return(private$distance_matrix(queryData, learning, learnVars, weights))
                        } else {
                          self$distMat
                        }
                      },
                      # get similar cases from reference data
                      calc_similar_cases = function(queryData, learning, nCases) {
                        if (class(self$distMat) != "matrix") {
                          stop("Need distance Matrix!")
                        }
                        
                        # fast ordering of similar cases
                        ordDist <- private$get_order(nCases)
                        # get most similar cases
                        similarCases <- do.call(rbind, apply(ordDist, 1,
                                                             function(x, data=learning) {
                                                               data[x, ]
                                                             }
                        )
                        )
                        self$distOrder <- ordDist
                        
                        # get distances
                        ordDist <- cbind(1:nrow(ordDist), ordDist)
                        distance <-  as.numeric(apply(ordDist, 1,
                              function(x, data=self$distMat) {
                                data[x[2:length(x)], x[1]]
                              }
                        ))
                        
                        # mark similar cases: 1:n ids
                        similarCases$caseId <- rep(1:nrow(queryData), each=nCases)
                        similarCases$scDist <- distance
                        self$similarCases <- similarCases
                      }),
                    private=list(
                      # calculate distance matrix; deprecated
                      distance_matrix = function (queryData, learning, learnVars, weights) {
                        trfData <- private$transform_data(queryData, learning, learnVars, weights)
                        # drop endpoints from reference
                        # now all columns of learning and queryData are numeric,
                        # s.t. we can now apply our rcpp function
                        return(
                          cbr:::get_Distance_Matrix(
                            trfData$newCases,
                            trfData$learning,
                            trfData$trafoweights
                          )
                        )
                      },
                      # calculate distance and return n nearest distance and
                      # row id of n nearest cases from reference data;
                      calc_kNN = function(newCases, learning, learnVars, weights, nCases) {
                        trfData <- private$transform_data(newCases, learning, learnVars, weights)
                        return(
                          cbr:::get_nearest_Elements(
                            x             = trfData$learning,
                            query         = trfData$newCases,
                            weights       = trfData$trafoweights,
                            sortDirection = 0L,
                            k             = nCases)
                        )
                      },
                      get_order = function(nCases) {
                        return(
                          cbr:::getOrderMatrix(
                            self$distMat,
                            nCases
                          )
                        )
                      },
                      transform_data = function(newCases, learning, learnVars, weights) {
                        # data preparation:
                        # we transform all factors to their corresponding
                        # weights and set weight equal to 1 for factor
                        # variables
                        nVars <- length(learnVars)
                        trafoweights <- rep(0, nVars)
                        for (j in 1:nVars) {
                          if (is.factor(learning[, learnVars[j]])) {
                            newCases[, learnVars[j]] <- weights[[learnVars[j]]][newCases[, learnVars[j]]]
                            learning[, learnVars[j]] <- weights[[learnVars[j]]][learning[, learnVars[j]]]
                            trafoweights[j] <- 1
                          } else { # else keep weights
                            trafoweights[j] <- weights[[learnVars[j]]]
                          }
                        }
                        names(trafoweights) <- NULL
                        return(list(newCases     = unname(as.matrix(newCases[, learnVars])),
                                    learning     = unname(as.matrix(learning[, learnVars])),
                                    trafoweights = trafoweights))
                      }
                    ))
