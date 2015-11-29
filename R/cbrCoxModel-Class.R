#' Cox Beta Model
#' 
#' Cox beta coefficients are use for building a weighted distance measure between 
#' the learning and verum data set. The learning data set is used for learning the
#' Cox model and use the obtained weights for calculating a (n x m)-distance 
#' matrix, where n is the number of cases in the learning data set and m is the 
#' number of cases of the verum data. This distance matrix can then be used for 
#' cluster analysis or for getting for each case in the verum data k (=1,...,l)
#' smilar cases from the learning data.
#' If verum data is ommitted, a n x n- distance matrix is returned.
#'
#' @param learning data set for learning the Cox model
#' @param verumData (optional): Verum data set. For each case in the verum data, 
#'  we are looking for the k (=1,…,l) similar cases. Learning and verum data set 
#'  need the same structure (variable names and scales)
#' @param learnVars (Default: all variables except endPoint): A character vector 
#' variable names. This variables are used for learning the model. Do not 
#' include time2event and event variable here. 
#' fitting.
#' @param endPoint (Default: c("Time2Event", "Event")): A character vector of 
#' length two. The first elements contains the variable name of the time 2 event 
#' variable and the second the name of the event variable. 
#' @param impute (Default: FALSE): Missing value imputation. Actually, not 
#' implemented for the Cox Model.
#'
#' @field new Initialization of the cbrCoxModel
#' 
#' @field learn Fit Cox model on the learning data set
#' 
#' @field getDistanceMatrix Calculates full n x m (n x n)-distance matrix 
#' 
#' @field getSimilarCases get for each case in verum data nCases (=1,...,l < n) 
#' similar cases from the learning data; (1:nCases matching)
#' 
#' @usage cbrCoxModel$new
#' 
#' @useDynLib cbr
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox-Beta
cbrCoxModel <- R6Class("cbrCoxModel",
                      inherit = cbrData,
                      public=list(
                        Weights    = NA,
                        distMat    = NA,
                        orderMat   = NA,
                        simCases   = NA,
                        coxFit     = NA,
                        cph        = NA,
                        valCox     = NA,
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
                          assign('dd', datadist(self$learning), pos='design.options')
                          options(datadist="dd")
                          
                          # formula and Cox-Model
                          formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                          coxFit <- cph(formel, data=self$learning, x=TRUE, y=TRUE, surv=T)
                          vars <- fastbw(coxFit, type = "i")
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
                        # fit cox model
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
                          assign('dd', rms::datadist(self$learning), pos='design.options')
                          options(datadist="dd")
                          
                          # formula and Cox-Model
                          formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                          coxFit <- rms::cph(formel, data=self$learning, x=TRUE, y=TRUE, surv=T)
                          self$coxFit <- coxFit
                          self$cph <- cox.zph(self$coxFit, "rank")
                          # self$valCox <- rms::validate(self$coxFit, B=200)
                          
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
                        getDistanceMatrix = function() {
                          # learn if weights are empty
                          if (class(self$Weights) != "list") {
                            self$learn()
                          }

                          # Start calculation
                          start <- Sys.time()
                          cat("Start calculating distance matrix...\n")
                          # get distance matrix
                          sc <- simCases$new()
                          self$distMat <- sc$getDistanceMatrix(self$verumData, self$learning, self$learnVars, self$Weights)
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
                          
                          if(private$check_weights()) {
                            stop("NA values in Cox Beta coefficients!")
                          }
                          
                          # check nCases input
                          if (missing(nCases))
                            nCases <- 1
                          if (!is.numeric(nCases))
                            stop("nCases must be numeric!")
                          if (nCases <= 0)
                            stop("nCases must be positive integer value!")

                          # catch floating numbers
                          nCases <- as.integer(nCases)
                          
                          self$getDistanceMatrix()
                          
                          # calculate distance and order of cases based on distance calculation
                          sc <- simCases$new(distMat=self$distMat)
                          sc$getSimilarCases(verumData=self$verumData, learning=self$learning, nCases=nCases)
                          self$orderMat <- sc$order
                          self$simCases <- sc$similarCases
                          # sc$getSimilarCases(self$verumData, self$learning, self$learnVars, self$Weights, nCases)
                          # self$distMat <- sc$distMat

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
                      ),
                      private = list(
                        # check weights on NA
                        check_weights = function() {
                          wNA <- unlist(lapply(self$Weights, function(x) any(is.na(x))))
                          if (any(wNA)) {
                            cat(paste0("Variables: ", names(wNA)[which(wNA)], " have NA weights.\n"))
                            return(TRUE)
                          }
                          return(FALSE)
                        }
                      )
)
