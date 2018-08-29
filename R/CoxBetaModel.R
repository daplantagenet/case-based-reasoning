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
#' @export
#' @format A \code{\link{R6Class}} generator object
#' 
CoxBetaModel <- R6Class(classname = "CoxBetaModel",
                        inherit = CBRBase,
                        public=list(
                          model       = 'cph',
                          model_param = list(x = T, y = T, surv = T),
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
                                ggplot2::geom_smooth(color="#2773ae", fill="#2773ae") +
                                ggplot2::ylab(paste0("Beta(t) of ", self$terms[i])) +
                                ggplot2::xlab("Time to Event") +
                                cowplot::background_grid(major="xy", minor="xy")
                              ggPlot <- c(ggPlot, list(g))
                            }
                            return(cowplot::plot_grid(plotlist = ggPlot,
                                                      ncol     = 2))
                          }
                        )
)