#' Root class for Regression Models, e.g., CPH, logistic, and linear regression
#' 
#' @keywords data-preparation
RegressionModel <- R6Class(classname = "RegressionModel",
                        inherit = CBRBase,
                        public=list(
                          weights     = NULL,
                          model       = '',
                          model_param = list(x = T, y = T),
                          model_fit   = NULL,
                          print = function() {
                            cat("Case-Based-Reasoning based Regression Beta Coefficients\n")
                            cat("---------------------------------------\n")
                            cat("Endpoints : ", paste(self$endPoint, collapse = ", "))
                            cat("Variables : ", paste(self$terms, collapse = ", "))
                            cat("Trained   : ", ifelse(is.null(self$weights), FALSE, TRUE))
                          },
                          # fast backward variable selection with penalization
                          variable_selection = function(dtData) {
                            dtData %>%
                              dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                            dtData <- private$check_data(dtData)
                            
                            # Timing
                            start <- Sys.time()
                            cat("Start learning...\n")
                            #  datadist scoping
                            regression_data <<- rms::datadist(dtData)
                            options(datadist="regression_data")
                            
                            # train regression model
                            func <- get(self$model, envir = as.environment('package:rms'))
                            params <- self$model_param
                            params$data <- dtData
                            params$formula <- self$formula
                            self$model_fit <- pryr::do_call(func, params)
                            
                            # Variable Selection
                            vars <- rms::fastbw(fit = self$model_fit, type = "i")
                            cat(paste0("Initial variable set: ", paste(c(self$endPoint, self$terms), collapse = ", "), "\n"))
                            cat(paste0("Selected variable set: ", paste(vars$names.kept, collapse = ", "), "\n"))
                            selected_vars <- c(self$endPoint, self$terms)
                            
                            # end timing
                            options(datadist=NULL)
                            end <- Sys.time()
                            duration <- round(as.numeric(end - start), 2)
                            cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                            
                            selected_vars
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
                            regression_data <<- rms::datadist(dtData)
                            options(datadist="regression_data")
                            
                            # train regression model
                            func <- get(self$model, envir = as.environment('package:rms'))
                            params <- self$model_param
                            params$data <- dtData
                            params$formula <- self$formula
                            self$model_fit <- pryr::do_call(func, params)
                            
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
                          transform_data = function(queryData, dtData, learnVars, weights) {
                            nVars <- length(learnVars)
                            trafoWeights <- rep(0, nVars)
                            for (j in 1:nVars) {
                              if (is.factor(dtData[[learnVars[j]]])) {
                                if (!is.null(queryData)) {
                                  queryData[[learnVars[j]]] <- weights[[learnVars[j]]][queryData[[learnVars[j]]]]
                                }
                                dtData[[learnVars[j]]] <- weights[[learnVars[j]]][dtData[[learnVars[j]]]]
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
                                        data         = unname(as.matrix(dtData[, learnVars, with=F])),
                                        trafoWeights = trafoWeights))
                          },
                          # calculate weighted absolute distance 
                          get_distance_matrix=function(dtData, queryData = NULL) {
                            if (is(dtData, "data.table")) {
                              dtData <- data.table::copy(dtData)
                            } else {
                              dtData <- data.table::copy(data.table::as.data.table(dtData))
                            }
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            testthat::expect_false(private$check_weights(), info = "NA values in regression beta coefficients!")
                            
                            if (is.null(queryData)) {
                              queryData <- data.table::copy(dtData)
                            } 
                            
                            # transform for weighted distance calculations
                            trData <- private$transform_data(queryData = queryData,  
                                                             dtData    = dtData, 
                                                             learnVars = self$terms, 
                                                             weights   = self$weights)
                            
                            # calculate distance matrix
                            self$distMat <- weightedDistance(x       = trData$data, 
                                                             y       = trData$queryData, 
                                                             weights = trData$trafoWeights) %>% 
                              as.matrix()
                          }
                        )
)