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
#' @param queryData (optional): Query data set. For each case in the query data,
#'  we are looking for the k (=1,…,l) similar cases in the learning data.
#'  Learning and query datasets need the same structure (variable names and scales)
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
#' @usage cbrCoxModel$new
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Beta
cbrCoxModel <- R6Class("cbrRegressionModel",
                              inherit = cbrData,
                              public=list(
                                weights    = NULL,
                                coxFit     = NULL,
                                cph        = NULL,
                                modelValid = NULL,
                                # fast backward variable selection with penalization
                                variable_selection = function() {
                                  # Timing
                                  start <- Sys.time()
                                  cat("Start learning...\n")

                                  #  datadist scoping
                                  on.exit(detach("design.options"))
                                  attach(list(), name="design.options")
                                  self$data %>%
                                    dplyr::select_(.dots = all.vars(self$formula)) -> dtData
                                  assign('dd', rms::datadist(dtData), pos='design.options')
                                  options(datadist="dd")

                                  # Cox Regression
                                  coxFit <- rms::cph(formula = self$formula,
                                                     data    = dtData,
                                                     x       = TRUE,
                                                     y       = TRUE,
                                                     surv    = T)

                                  # Variable Selection
                                  vars <- rms::fastbw(fit  = coxFit,
                                                      type = "i")
                                  cat(paste0("Initial variable set: ", paste(all.vars(self$formula), collapse = ", "), "\n"))
                                  cat(paste0("Selected variable set: ", paste(vars$names.kept, collapse = ", "), "\n"))
                                  vars <- all.vars(self$formula)
                                  self$formula <- as.formula(paste0("Surv(", vars[1], ", ", vars[2], "~", paste(vars$names.kept, collapse = "+")))

                                  # end timing
                                  options(datadist=NULL)
                                  end <- Sys.time()
                                  duration <- round(as.numeric(end - start), 2)
                                  cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                                },
                                # fit model
                                learn = function() {
                                  # Timing
                                  start <- Sys.time()
                                  cat("Start learning...\n")
                                  #  datadist scoping
                                  on.exit(detach("design.options"))
                                  attach(list(), name="design.options")
                                  self$data %>%
                                    dplyr::select_(.dots = all.vars(self$formula)) -> dtData
                                  assign('dd', rms::datadist(dtData), pos='design.options')
                                  options(datadist="dd")

                                  # Cox Regression
                                  coxFit <- rms::cph(formula = self$formula,
                                                     data    = dtData,
                                                     x       = TRUE,
                                                     y       = TRUE,
                                                     surv    = T)
                                  self$coxFit <- coxFit
                                  self$cph <- survival::cox.zph(self$coxFit, "rank")
                                  
                                  vars <- all.vars(self$formula)[-c(1, 2)]
                                  nVars <- length(vars) 
                                  weights <- vector("list", nVars)
                                  names(weights) <- vars
                                  # get weights
                                  for (i in 1:nVars) {
                                    if (is.factor(self$data[[vars[i]]])) {
                                      nLev <- nlevels(self$data[[vars[i]]])
                                      weightsTmp <- rep(NA, times = nLev)
                                      names(weightsTmp) <- levels(self$data[[vars[i]]])
                                      for (j in 1:nLev) {
                                        myLevel <- paste(vars, "=", levels(self$data[[vars[i]]])[j], sep="")
                                        if (j==1) {
                                          weightsTmp[j] <- 0
                                        } else {
                                          weightsTmp[j] <- coxFit$coefficients[myLevel]
                                        }
                                      }
                                      weights[[i]] <- weightsTmp
                                    } else {  # else Faktor numeric
                                      myLevel <- paste(vars[i])
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
                                  vars <- all.vars(self$formula)[-c(1, 2)]
                                  n <- length(vars)
                                  ggPlot <- list()
                                  for (i in 1:n) {
                                    df <- data.frame(x=self$cph$x, y=self$cph$y[, i])
                                    g <- ggplot2::ggplot(df, aes(x=x, y=y)) +
                                      ggplot2::geom_hline(yintercept=0, colour="grey") +
                                      ggplot2::geom_point() +
                                      ggplot2::geom_smooth(color="#2773ae", fill="#2773ae") +
                                      ggplot2::ylab(paste0("Beta(t) of ", vars[i])) +
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
                                # calculate weighted absolute distance 
                                get_distance_matrix=function() {
                                  if (is.null(self$weights)) {
                                    self$learn()
                                  }
                                  
                                  # learn if weights are empty
                                  if (is.null(self$weights))
                                    self$learn()
                                  
                                  if(private$check_weights()) {
                                    stop("NA values in regression beta coefficients!")
                                  }
                                  
                                  self$distMat <- Similarity::wDistance(x       = self$data, 
                                                                        y       = self$queryData, 
                                                                        weights = self$weights) %>% 
                                    as.matrix()
                                }
                              )
)
