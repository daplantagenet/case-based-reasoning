#' Case Based Reasoning with Cox Model for distance calculation
#'
#' @useDynLib cbr
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
                        coxFit     = NA,
                        cph        = NA,
                        learn = function() {
                          if (!private$learningValid)
                            stop("Reference data is not valid!")

                          # Timing
                          start <- Sys.time()
                          cat("Start learning...\n")
                          #  datadist scoping
                          on.exit(detach("design.options"))
                          attach(list(), name="design.options")
                          assign('dd', datadist(self$learning), pos='design.options')
                          options(datadist="dd")
                          
                          # formula and Cox-Model
                          formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                          coxFit <- cph(formel, data=self$learning, x=TRUE, y=TRUE, surv=T)
                          self$coxFit <- coxFit
                          self$cph <- cox.zph(self$coxFit, "rank")
                          
                          nVars <- length(self$learnVars)
                          Weights <- vector("list", nVars)
                          names(Weights) <- self$learnVars
                          # get weights
                          for (i in 1:nVars) {
                            if (is.factor(self$learning[, self$learnVars[i]])) {
                              nLev <- nlevels(self$learning[, self$learnVars[i]])
                              weights <- rep(NA, times = nLev)
                              names(weights) <- levels(self$learning[, self$learnVars[i]])
                              for (j in 1:nLev) {
                                myLevel <- paste(self$learnVars[i], "=", levels(self$learning[, self$learnVars[i]])[j], sep="")
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
                          options(datadist=NULL)
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                        },
                        # checking linearity of numeric variables
                        check_linearity = function() {
                          # learn if weights are empty
                          if (class(self$Weights) != "list") {
                            self$learn()
                          }
                          
                          # which variables are numeric
                          varClass <- unlist(lapply(self$learning[self$learnVars], class))
                          idNum <- which(varClass %in% "numeric")
                          if (length(idNum) == 0) {
                            return()
                          }
                          
                          #  datadist scoping
                          on.exit(detach("design.options"))
                          attach(list(), name="design.options")
                          assign('dd', datadist(self$learning), pos='design.options')
                          options(datadist="dd")
                          
                          # residual plots
                          ggPlot <- list()
                          for (i in idNum) {
                            formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", self$learnVars[i]))
                            fit <- cph(formel, data=self$learning, x=T, y=T)
                            self$learning$res <- residuals(fit, "martingale")
                            var <- self$learnVars[i]
                            g <- ggplot(self$learning, aes_string(x=var, y="res")) +
                              geom_hline(yintercept=0, colour="grey") +
                              geom_point() +
                              geom_smooth(color="#2773ae", fill="#2773ae") +
                              ylab("Martingal Residuen") + xlab(var) +
                              background_grid(major="xy", minor="xy")
                            ggPlot <- c(ggPlot, list(g))
                          }
                          self$learning$res <- NULL
                          options(datadist=NULL)
                          return(plot_grid(plotlist = ggPlot, 
                                           ncol     = 2))
                        },
                        # check proportional hazard
                        check_ph=function() {
                          # learn if weights are empty
                          if (class(self$Weights) != "list") {
                            self$learn()
                          }
                          
                          n <- length(self$learnVars)
                          ggPlot <- list()
                          for (i in 1:n) {
                            df <- data.frame(x=self$cph$x, y=self$cph$y[, i])
                            g <- ggplot(df, aes(x=x, y=y)) +
                              geom_hline(yintercept=0, colour="grey") +
                              geom_point() +
                              geom_smooth(color="#2773ae", fill="#2773ae") +
                              ylab(paste0("Beta(t) of ", self$learnVars[i])) + xlab("Time to Event") +
                              background_grid(major="xy", minor="xy")
                            ggPlot <- c(ggPlot, list(g))
                          }
                          
                          return(plot_grid(plotlist = ggPlot, 
                                           ncol     = 2))
                        },
                        # calculate distance matrix for verum data
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
                          self$distMat <- sc$getFullDistanceMatrix(self$verumData, self$learning, self$learnVars, self$Weights)
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                        },
                        # get verum data, if there are missing values, return 
                        # imputed data
                        getVerumData = function () {
                          return(self$verumData)
                        },
                        # get learning data, if it is imputed return imputed data
                        # else data.frame without missing cases
                        getLearningData = function () {
                          return(self$learning)
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
                          sc$getSimilarCases(self$verumData, self$learning, self$learnVars, self$Weights, nCases)
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
                          return(valSC$validate(self$verumData, self$simCases, self$learnVars, plot))
                        }
                      )
)
