#' Cox-Beta Model for Case-Based-Reasoning
#'
#' Regression beta coefficients obtained from a CPH regression model fitted on the 
#' training data are used for building a weighted distance measure between
#' train and test data. Afterwards, we will use these weights for calculating a 
#' (n x m)-distance matrix, where n is the number of observations in the training data, 
#' and m is the number of observations of the test data. The user can use this 
#' distance matrix for further cluster analysis or for extracting for each test observation 
#' k (= 1,...,l) similar cases from the train data. We use the rms-package for model fitting,
#' variable selection, and checking model assumptions.
#' If the user omits the test data, this functions returns a n x n-distance matrix.
#'
#' @section Usage:
#' For usage details see \bold{Methods, Arguments, and Examples} sections.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(formula, ...)}}{This method is used to create an
#'   object of this class \code{CoxBetaModel}. Formula for analysis has to be 
#'   provided.}
#'   \item{\code{fit(dtData)}}{Fits the CPH model.}
#'   \item{...}{See \link{CBRBase} class.}
#'   }
#'
#' @docType class
#' @importFrom R6 R6Class
<<<<<<< HEAD
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
=======
#' @export
#' @format A \code{\link{R6Class}} generator object
#' 
CoxBetaModel <- R6Class(classname = "CoxBetaModel",
                        inherit = CBRBase,
                        public=list(
                          model       = 'cph',
                          model_param = list(x = T, y = T, surv = T),
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
                          # check proportional hazard
                          check_ph=function() {
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            n <- length(self$terms)
                            ggPlot <- list()
                            zph <- survival::cox.zph(self$model_fit, "rank")
                            for (i in 1:n) {
                              df <- data.frame(x=zph$x, y=zph$y[, i])
                              g <- ggplot2::ggplot(df, aes(x=x, y=y)) +
                                ggplot2::geom_hline(yintercept=0, colour="grey") +
                                ggplot2::geom_point() +
                                ggplot2::geom_smooth(color="#18BC9C", fill="#18BC9C") +
                                ggplot2::ylab(paste0("Beta(t) of ", self$terms[i])) +
                                ggplot2::xlab("Time to Event") +
                                cowplot::background_grid(major="xy", minor="xy")
                              ggPlot <- c(ggPlot, list(g))
                            }
                            return(cowplot::plot_grid(plotlist = ggPlot,
                                                      ncol     = 2))
                          }
<<<<<<< HEAD
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
=======
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
                        )
)