#' R6 class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords Cox Model
cbrRFProxy <- R6Class("cbrRFProxy",
                      inherit = cbrData,
                      public=list(
                        distMat    = NA,
                        orderMat   = NA,
                        simCases   = NA,
                        impData    = NA,
                        learn=function(nCores, ntree, mtry, splitrule, ntime, nsplit, verbose) {

                          # split rule
                          if (missing(splitrule)) {
                            splitrule <- "logrank"
                          }
                          if (!splitrule %in% c("logrank", "logrankscore"))
                            stop("splitrule should be: logrank or logrankscore")

                          # verbose
                          if (missing(verbose)) {
                            verbose <- FALSE
                          }

                          # tree
                          if (missing(ntree)) {
                            ntree <- 300
                          }

                          # mtry
                          if (missing(mtry)) {
                            mtry <- length(self$learnVars)
                          }

                          # ntime
                          if (missing(ntime)) {
                            ntime <- NULL
                          }

                          # nsplit
                          if (missing(nsplit)) {
                            nsplit <- 0
                          }

                          # number of cores for calculation
                          if (missing(nCores)) {
                            options(rf.cores=detectCores() - 1, mc.cores=detectCores() - 1)
                          } else {
                            nCores <- as.integer(nCores)
                            if (nCores >= detectCores()) {
                              nCores <- detectCores() - 1
                            }
                            options(rf.cores=nCores, mc.cores=nCores)
                          }

                          # Timing
                          start <- Sys.time()
                          cat("Start learning...\n")

                          # new data available?
                          variables <- c(self$endPoint, self$learnVars)
                          if (self$refEQNew) {
                            learnData <- self$learning[, variables]
                          } else {
                            learnData <- rbind(self$learning[, variables], self$verumData[, variables])
                          }

                          # impute
                          if (self$impute) {
                            impute <- "na.impute"
                          } else {
                            impute <- "na.omit"
                          }

                          # Learning
                          formel <- as.formula(paste0("Surv(", self$endPoint[1],", ", self$endPoint[2], ") ~ ", paste(self$learnVars, collapse="+")))
                          rsf <- rfsrc(formel,
                                       data       = learnData,
                                       ntree      = ntree,
                                       mtry       = mtry,
                                       splitrule  = splitrule,
                                       proximity  = "all",
                                       na.action  = impute,
                                       importance = "none",
                                       do.trace   = verbose)
                          plot(rsf)
                          
                          # if imputation is activated, then save it
                          if (self$impute) {
                            self$impData <- rsf$imputed.data
                          } else {
                            self$impData <- NA
                          }
                          
                          # get distance matrix: rsf$proximity has dimension n x n.
                          # n = nRef + nNew
                          # Dimension distance matrix:
                          # rows: reference cases
                          # columns: new cases
                          if (self$refEQNew) {
                            self$distMat <- rsf$proximity
                          } else {
                            nRef <- nrow(self$learning)
                            self$distMat <- rsf$proximity[1:nRef, (nRef + 1):ncol(rsf$proximity)]
                          }
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                        },
                        getFullDistanceMatrix = function() {
                          if (class(self$distMat) != "matrix") {
                            self$learn()
                          }
                        },
                        getSimilarCases = function(nCases) {
                          if (self$refEQNew) {
                            stop("no new data!")
                          }

                          start <- Sys.time()
                          cat("Start calculating similar cases...\n")
                          # learn if weights are empty
                          if (class(self$distMat) != "matrix") {
                            self$learn()
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
                          sc <- simCases$new(distMat=self$distMat, method="rfProxy")
                          sc$getSimilarCases(self$verumData, self$learning, self$learnVars, nCases=nCases)
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
                      ))
