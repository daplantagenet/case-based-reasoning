#' Cox-Regression Model
#'
#' Regression beta coefficients are use for building a weighted distance measure between
#' the learning and verum data set. The learning data set is used for learning the Cox
#' model and use the obtained weights for calculating a (n x m)-distance
#' matrix, where n is the number of cases in the learning data set and m is the
#' number of cases of the query data. This distance matrix can then be used for
#' cluster analysis or for getting for each case in the query data k (=1,...,l)
#' smilar cases from the learning data. The rms-package is used for model fitting,
#' variable selection, and checking the assumptions.
#' If query data is ommitted, a n x n- distance matrix is returned.
#'
#' @param formula : formula for learning the Cox model
#' @param data    : the dataset for learning the model
#' @param queryData (optional) : Query data set. For each case in the query data,
#'  we are looking for the k (=1,…,l) similar cases in the learning data.
#'  Learning and query datasets need the same structure (variable names and scales)
#'
#' @field new Initialization of the cbrRegressionModel
#'
#' @field variable_selection : performs a fast backward variable selection
#'
#' @field learn Fit cox model on the learning data set
#'
#' @field check_ph Check proportional hazard assumption
#'
#' @field calc_distance_matrix Calculates full n x m / (n x n)-distance matrix
#'
#' @field calc_similar_cases get for each case in verum data nCases (=1,...,l < n)
#' similar cases from the learning data; (1:nCases matching)
#'
#' @usage cbrCoxModel$new
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords Beta
#' @name CoxBetaModel
#' @export
CoxBetaModel <- R6Class(classname = "CoxBetaModel",
                        inherit = CBRBase,
                        public=list(
                          weights    = NULL,
                          coxFit     = NULL,
                          cph        = NULL,
                          modelValid = NULL,
                          # fast backward variable selection with penalization
                          variable_selection = function(dtData) {
                            dtData %>%
                              dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                            dtData <- private$check_data(dtData)
                            
                            # Timing
                            start <- Sys.time()
                            cat("Start learning...\n")
                            #  datadist scoping
                            on.exit(detach("design.options"))
                            attach(list(), name="design.options")
                            assign('cbrCoxModel_data', rms::datadist(dtData), pos='design.options')
                            options(datadist="cbrCoxModel_data")
                            
                            # Cox Regression
                            dtData %>% 
                              rms::cph(formula = self$formula, data = ., x = TRUE, y = TRUE, surv = T) -> self$coxFit
                            
                            # Variable Selection
                            vars <- rms::fastbw(fit = coxFit, type = "i")
                            cat(paste0("Initial variable set: ", paste(c(self$endPoint, self$terms), collapse = ", "), "\n"))
                            cat(paste0("Selected variable set: ", paste(vars$names.kept, collapse = ", "), "\n"))
                            vars <- c(self$endPoint, self$terms)
                            self$formula <- as.formula(paste0("Surv(", vars[1], ", ", vars[2], "~", paste(vars$names.kept, collapse = "+")))
                            
                            # end timing
                            options(datadist=NULL)
                            end <- Sys.time()
                            duration <- round(as.numeric(end - start), 2)
                            cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                          },
                          # fit model
                          fit = function(dtData) {
                            dtData %>%
                              dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                            dtData <- private$check_data(dtData)
                            
                            # Timing
                            start <- Sys.time()
                            cat("Start learning...\n")
                            #  datadist scoping
                            on.exit(detach("design.options"))
                            attach(list(), name="design.options")
                            assign('cbrCoxModel_data', rms::datadist(dtData), pos='design.options')
                            options(datadist="cbrCoxModel_data")
                            
                            # Cox Regression
                            dtData %>% 
                              rms::cph(formula = self$formula, data = ., x = TRUE, y = TRUE, surv = T) -> self$coxFit
                            self$cph <- survival::cox.zph(self$coxFit, "rank")
                            
                            nVars <- length(self$terms) 
                            weights <- vector("list", nVars)
                            names(weights) <- self$terms
                            # get weights
                            for (i in 1:nVars) {
                              if (is.factor(dtData[[self$terms[i]]])) {
                                nLev <- nlevels(dtData[[self$terms[i]]])
                                weightsTmp <- rep(NA, times = nLev)
                                names(weightsTmp) <- levels(dtData[[self$terms[i]]])
                                for (j in 1:nLev) {
                                  myLevel <- paste(self$terms[i], "=", levels(dtData[[self$terms[i]]])[j], sep="")
                                  if (j==1) {
                                    weightsTmp[j] <- 0
                                  } else {
                                    weightsTmp[j] <- self$coxFit$coefficients[myLevel]
                                  }
                                }
                                weights[[i]] <- weightsTmp
                              } else {  # else numeric
                                myLevel <- paste(self$terms[i])
                                weights[[i]] <- self$coxFit$coefficients[myLevel]
                              }
                            }
                            self$weights <- weights
                            # end timing
                            options(datadist=NULL)
                            end <- Sys.time()
                            duration <- round(as.numeric(end - start), 2)
                            cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                          },
                          # check proportional hazard
                          check_ph=function() {
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            n <- length(self$terms)
                            ggPlot <- list()
                            for (i in 1:n) {
                              df <- data.frame(x=self$cph$x, y=self$cph$y[, i])
                              g <- ggplot2::ggplot(df, aes(x=x, y=y)) +
                                ggplot2::geom_hline(yintercept=0, colour="grey") +
                                ggplot2::geom_point() +
                                ggplot2::geom_smooth(color="#2773ae", fill="#2773ae") +
                                ggplot2::ylab(paste0("Beta(t) of ", self$terms[i])) +
                                ggplot2::xlab("Time to Event") +
                                cowplot::background_grid(major="xy", minor="xy")
                              ggPlot <- c(ggPlot, list(g))
                            }
                            return(cowplot::plot_grid(plotlist = ggPlot,
                                                      ncol     = 2))
                          }
                        ),
                        private = list(
                          # check weights on NA
                          check_weights = function() {
                            wNA <- unlist(lapply(self$weights, function(x) any(is.na(x))))
                            if (any(wNA)) {
                              cat(paste0("Variables: ", names(wNA)[which(wNA)], " have NA weights.\n"))
                              return(TRUE)
                            }
                            return(FALSE)
                          },
                          # transform_data:
                          # we transform all factors to their corresponding
                          # weights and set weight equal to 1 for factor variables
                          transform_data = function(queryData, data, learnVars, weights) {
                            nVars <- length(learnVars)
                            trafoWeights <- rep(0, nVars)
                            for (j in 1:nVars) {
                              if (is.factor(data[[learnVars[j]]])) {
                                if (!is.null(queryData)) {
                                  queryData[[learnVars[j]]] <- weights[[learnVars[j]]][queryData[[learnVars[j]]]]
                                }
                                data[[learnVars[j]]] <- weights[[learnVars[j]]][data[[learnVars[j]]]]
                                trafoWeights[j] <- 1
                              } else { # else keep weights
                                trafoWeights[j] <- weights[[learnVars[j]]]
                              }
                            }
                            names(trafoWeights) <- NULL
                            
                            if(is.null(queryData)) {
                              queryData <- NULL
                            } else {
                              queryData <- unname(as.matrix(queryData[, learnVars, with=F]))
                            }
                            return(list(queryData    = queryData,
                                        data         = unname(as.matrix(data[, learnVars, with=F])),
                                        trafoWeights = trafoWeights))
                          },
                          # calculate weighted absolute distance 
                          get_distance_matrix=function(dtData, queryData = NULL) {
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            testthat::expect_false(private$check_weights(), info = "NA values in regression beta coefficients!")
                            
                            if (!is.null(self$queryData)) {
                              queryData <- data.table::copy(queryData)
                            } 
                            
                            # transform for weighted distance calculations
                            trData <- private$transform_data(queryData = queryData, 
                                                             data      = data.table::copy(dtData), 
                                                             learnVars = self$terms, 
                                                             weights   = self$weights)
                            
                            # calculate distance matrix
                            self$distMat <- CaseBasedReasoning:::wDistanceXYCPP(x       = trData$data, 
                                                                                y       = trData$queryData, 
                                                                                weights = trData$trafoWeights) %>% 
                              as.matrix()
                          }
                        )
)
