simCases <- R6Class("simCases",
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
                      },
                      # get similar cases from reference data
                      getSimilarCases = function(newData, nCases) {
                        if (missing(newData))
                          stop("New data is missing!")

                        # validation check new data
                        self$newData <- private$check_data(newData, isReference=F)

                        start <- Sys.time()
                        cat("Start caclulating similar cases...\n")
                        # learn if weights are empty
                        if (class(self$Weights) != "list")
                          self$learn()

                        # check nCases input
                        if (missing(nCases))
                          nCases <- 1
                        if (!is.numeric(nCases))
                          stop("nCases must be numeric!")
                        if (nCases <= 0)
                          stop("nCases must be positive integer value!")

                        # catch floating numbers
                        nCases <- as.integer(nCases)

                        # calculate distance and order of cases based on distance calculation
                        ordDist <- private$calcNDist(newData, self$refData, self$learnVars, self$Weights, nCases)
                        self$distMat <- ordDist$distance

                        # get most similar cases
                        similarCases <- do.call(rbind, apply(ordDist$order, 2,
                                                             function(x, data=self$refData) {
                                                               data[x, ]
                                                             }
                        )
                        )
                        # mark similar cases: 1:n ids
                        similarCases$caseId <- rep(1:nrow(newData), each=nCases)
                        # get distances
                        # distList <- apply(ordDist$distance, 2, list)
                        # similarCases$distance <- unlist(lapply(distList, function(x, n=nCases) {or <- order(x[[1]]);x[[1]][or[1:n]]}))
                        end <- Sys.time()
                        duration <- round(as.numeric(end - start), 2)
                        cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                        return(similarCases)
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
                        return(.Call("cbr_get_nearest_Elements",
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
                      }
                    ))