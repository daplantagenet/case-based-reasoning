#' Regression Model
#' 
#' Regression beta coefficients are use for building a weighted distance measure between 
#' the learning and verum data set. The learning data set is used for learning the
#' model (OLS, LR, and Cox) and use the obtained weights for calculating a (n x m)-distance 
#' matrix, where n is the number of cases in the learning data set and m is the 
#' number of cases of the query data. This distance matrix can then be used for 
#' cluster analysis or for getting for each case in the query data k (=1,...,l)
#' smilar cases from the learning data. The rms-package is used for model fitting,
#' variable selection, and checking the assumptions. 
#' If query data is ommitted, a n x n- distance matrix is returned.
#'
#' @param learning data set for learning the Cox model
#' @param queryData (optional): Query data set. For each case in the query data, 
#'  we are looking for the k (=1,…,l) similar cases in the learning data. 
#'  Learning and query datasets need the same structure (variable names and scales)
#' @param learnVars (Default: all variables except endPoint): A character vector 
#' variable names. This variables are used for learning the model. Do not 
#' include time2event and event variable here. 
#' fitting.
#' @param endPoint (Default: c("Time2Event", "Event")): A character vector with
#' the name of the dependent variable
#' @param impute (Default: FALSE): Missing value imputation. Actually, not 
#' implemented for the regression model.
#'
#' @field new : Initialization of the cbrRegressionModel
#' 
#' @field variable_selection : performs a fast backward variable selection
#' 
#' @field learn : Fit regression model on the learning data set
#' 
#' @field check_ph : Check proportional hazard assumption
#' 
#' @field calc_distance_matrix : Calculates full n x m / (n x n)-distance matrix 
#' 
#' @field calc_similar_cases : get for each case in verum data nCases (=1,...,l < n) 
#' similar cases from the learning data; (1:nCases matching)
#' 
#' @usage cbrRegressionModel$new
#' 
#' @useDynLib cbr
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Beta
cbrRegressionModel <- R6Class("cbrRegressionModel",
                              inherit = cbrData,
                              public=list(
                                weights    = NULL,
                                distMat    = NULL,
                                orderMat   = NULL,
                                simCases   = NULL,
                                coxFit     = NULL,
                                cph        = NULL,
                                modelValid = NULL,
                                # fast backward variable selection with penalization
                                variable_selection = function() {
                                  if (!private$learningValid)
                                    stop("Reference data is not valid!")
                                  # Timing
                                  start <- Sys.time()
                                  cat("Start learning...\n")
                                  
                                  #  datadist scoping
                                  on.exit(detach("design.options"))
                                  attach(list(), name="design.options")
                                  assign('dd', rms::datadist(self$learning), pos='design.options')
                                  options(datadist="dd")
                                  
                                  # Cox Regression
                                  formula <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                                  coxFit <- rms::cph(formula = formula, 
                                                     data    = self$learning, 
                                                     x       = TRUE, 
                                                     y       = TRUE, 
                                                     surv    = T)
                                  
                                  # Variable Selection
                                  vars <- rms::fastbw(fit  = coxFit, 
                                                      type = "i")
                                  cat(paste0("Initial variable set: ", paste(self$learnVars, collapse = ", "), "\n"))
                                  cat(paste0("Selected variable set: ", paste(vars$names.kept, collapse = ", "), "\n"))
                                  self$info$y[5] <- paste(vars$names.kept, collapse = ", ")
                                  self$learnVars <- vars$names.kept
                                  
                                  # end timing
                                  options(datadist=NULL)
                                  end <- Sys.time()
                                  duration <- round(as.numeric(end - start), 2)
                                  cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                                },
                                # fit model
                                learn = function() {
                                  if (!private$learningValid)
                                    stop("Reference data is not valid!")
                                  
                                  self$info$y[4] <- paste(self$learnVars, collapse = ", ")
                                  
                                  # Timing
                                  start <- Sys.time()
                                  cat("Start learning...\n")
                                  #  datadist scoping
                                  on.exit(detach("design.options"))
                                  attach(list(), name="design.options")
                                  self$learning %>% 
                                    dplyr::select_(.dots = c(self$endPoint, self$learnVars)) -> dtData
                                  assign('dd', rms::datadist(dtData), pos='design.options')
                                  options(datadist="dd")
                                  
                                  # Cox Regression
                                  formula <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                                  coxFit <- rms::cph(formula = formula, 
                                                     data    = dtData, 
                                                     x       = TRUE, 
                                                     y       = TRUE, 
                                                     surv    = T)
                                  self$coxFit <- coxFit
                                  self$cph <- survival::cox.zph(self$coxFit, "rank")
                                  
                                  nVars <- length(self$learnVars)
                                  weights <- vector("list", nVars)
                                  names(weights) <- self$learnVars
                                  # get weights
                                  for (i in 1:nVars) {
                                    if (is.factor(self$learning[, self$learnVars[i]])) {
                                      nLev <- nlevels(self$learning[, self$learnVars[i]])
                                      weightsTmp <- rep(NA, times = nLev)
                                      names(weightsTmp) <- levels(self$learning[, self$learnVars[i]])
                                      for (j in 1:nLev) {
                                        myLevel <- paste(self$learnVars[i], "=", levels(self$learning[, self$learnVars[i]])[j], sep="")
                                        if (j==1) {
                                          weightsTmp[j] <- 0
                                        } else {
                                          weightsTmp[j] <- coxFit$coefficients[myLevel]
                                        }
                                      }
                                      weights[[i]] <- weightsTmp
                                    } else {  # else Faktor numeric
                                      myLevel <- paste(self$learnVars[i])
                                      weights[[i]] <- coxFit$coefficients[myLevel]
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
                                  if (!is(self$weights, "list")) {
                                    self$learn()
                                  }
                                  n <- length(self$learnVars)
                                  ggPlot <- list()
                                  for (i in 1:n) {
                                    df <- data.frame(x=self$cph$x, y=self$cph$y[, i])
                                    g <- ggplot2::ggplot(df, aes(x=x, y=y)) +
                                      ggplot2::geom_hline(yintercept=0, colour="grey") +
                                      ggplot2::geom_point() +
                                      ggplot2::geom_smooth(color="#2773ae", fill="#2773ae") +
                                      ggplot2::ylab(paste0("Beta(t) of ", self$learnVars[i])) + 
                                      ggplot2::xlab("Time to Event") +
                                      cowplot::background_grid(major="xy", minor="xy")
                                    ggPlot <- c(ggPlot, list(g))
                                  }
                                  return(cowplot::plot_grid(plotlist = ggPlot, 
                                                            ncol     = 2))
                                },
                                # calculate distance matrix
                                calc_distance_matrix = function() {
                                  # learn if weights are empty
                                  if (!is(self$weights, "list")) {
                                    self$learn()
                                  }
                                  # Start calculation
                                  start <- Sys.time()
                                  cat("Start calculating distance matrix...\n")
                                  # get distance matrix
                                  sc <- simCases$new()
                                  self$distMat <- sc$get_distance_matrix(self$queryData, self$learning, self$learnVars, self$weights)
                                  end <- Sys.time()
                                  duration <- round(as.numeric(end - start), 2)
                                  cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                                },
                                # get query data, if there are missing values, 
                                # return imputed data
                                get_query_data = function () {
                                  return(self$queryData)
                                },
                                # get learning data, if it is imputed return imputed data
                                # else data.frame without missing cases
                                get_data = function () {
                                  return(self$learning)
                                },
                                get_sim_cases = function() {
                                  return(self$simCases)
                                },
                                # return query + matched data
                                get_matched_data = function() {
                                  queryData <- self$queryData
                                  queryData$group <- "Query Data"
                                  matchedData <- self$simCases
                                  matchedData$caseId <- NULL
                                  matchedData$scDist <- NULL
                                  matchedData$group <- "Matched Data"
                                  return(rbind(queryData, simCases))
                                },
                                # get similar cases from reference data
                                calc_similar_cases = function(k = 1) {
                                  if (self$refEQNew) {
                                    stop("no new data!")
                                  }
                                  start <- Sys.time()
                                  cat("Start caclulating similar cases...\n")
                                  # learn if weights are empty
                                  if (class(self$weights) != "list")
                                    self$learn()
                                  
                                  if(private$check_weights()) {
                                    stop("NA values in regression beta coefficients!")
                                  }
                                  # check nCases input
                                  if (!is.numeric(k))
                                    stop("nCases must be numeric!")
                                  if (k <= 0)
                                    stop("nCases must be positive integer value!")
                                  # catch floating numbers
                                  k <- as.integer(k)
                                  self$calc_distance_matrix()
                                  # calculate distance and order of cases based on distance calculation
                                  sc <- simCases$new(distMat=self$distMat)
                                  sc$calc_similar_cases(queryData = self$queryData, 
                                                        learning  = self$learning, 
                                                        nCases    = k)
                                  self$orderMat <- sc$order
                                  self$simCases <- sc$similarCases
                                  end <- Sys.time()
                                  duration <- round(as.numeric(end - start), 2)
                                  cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                                },
                                validate_model = function(plot=T) {
                                  if (is.null(nrow(self$simCases)))
                                    stop("no similar cases")
                                  valSC <- cbrValidate$new()
                                  return(valSC$validate(self$queryData, 
                                                        self$simCases, 
                                                        self$learnVars, 
                                                        plot))
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
                                }
                              )
)
