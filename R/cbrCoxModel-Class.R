#' Case Based Reasoning with Cox Model for distance calculation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrCoxModel <- R6Class("cbrCoxModel",
                      inherit = cbrData,
                      public=list(
                        Weights    = NA,
                        distMat    = NA,
                        orderMat   = NA,
                        simCases   = NA,
                        learn = function() {
                          if (!private$refDataValid)
                            stop("Reference data is not valid!")

                          # Timing
                          start <- Sys.time()
                          cat("Start learning...\n")
                          # formula and Cox-Model
                          formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                          coxFit <- coxph(formel, data=self$refData)
                          nVars <- length(self$learnVars)
                          Weights <- vector("list", nVars)
                          names(Weights) <- self$learnVars
                          # get weights
                          for (i in 1:nVars) {
                            if (is.factor(self$refData[, self$learnVars[i]])) {
                              nLev <- nlevels(self$refData[, self$learnVars[i]])
                              weights <- rep(NA, times = nLev)
                              names(weights) <- levels(self$refData[, self$learnVars[i]])
                              for (j in 1:nLev) {
                                myLevel <- paste(self$learnVars[i], levels(self$refData[, self$learnVars[i]])[j], sep="")
                                if (j==1) {
                                  weights[j] <- 0
                                } else {
                                  weights[j] <- coxFit$coefficients[myLevel]
                                }
                              }
                              Weights[[i]] <- weights
                            } else {  # else Faktor numeric
                              myLevel <- paste(self$learnVars[i])
                              Weights[[i]] <- coxFit$coefficients[myLevel]
                            }
                          }
                          self$Weights <- Weights

                          # end timing
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                        },
                        # calculate distance matrix for new data
                        getFullDistanceMatrix = function() {
                          # learn if weights are empty
                          if (class(self$Weights) != "list") {
                            self$learn()
                          }

                          # Start calculation
                          start <- Sys.time()
                          cat("Start calculating distance matrix...\n")
                          # get distance matrix
                          sc <- simCases$new(method="cox")
                          self$distMat <- sc$getFullDistanceMatrix(self$newData, self$refData, self$learnVars, self$Weights)
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                        },
                        # get similar cases from reference data
                        getSimilarCases = function(nCases) {
                          if (self$refEQNew) {
                            stop("no new data!")
                          }

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
                          sc <- simCases$new()
                          sc$getSimilarCases(self$newData, self$refData, self$learnVars, self$Weights, nCases)
                          self$distMat <- sc$distMat
                          self$orderMat <- sc$order
                          self$simCases <- sc$similarCases

                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                        },
                        validate = function(plot=T) {
                          if (is.null(nrow(self$simCases)))
                            stop("no similar cases")
                          valSC <- cbrValidate$new()
                          return(valSC$validate(self$newData, self$simCases, self$learnVars, plot))
                        }
                      )
)
