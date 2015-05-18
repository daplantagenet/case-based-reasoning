cbrRFProxy <- R6Class("cbrRFProxy",
                      inherit = cbrData,
                      public=list(
                        distMat    = NA,
                        orderMat   = NA,
                        simCases   = NA,
                        learn=function(nCores="max", ntree=100, mtry=3, splitrule="logrank", ntime=NULL, nsplit=0) {
                          # split rule
                          if (splitrule %in% c("logrank", "logrankscore"))
                            stop("splitrule should be: logrank or logrankscore")

                          # number of cores for calculation
                          if (nCores=="max") {
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
                          if (self$refEQNew) {
                            learnData <- self$refData
                          } else {
                            learnData <- cbind(self$refData, sel$newData)
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
                                       nsplit     = nsplit,
                                       splitrule  = "logrank",
                                       proximity  = "all",
                                       na.action  = "na.impute",
                                       importance = "none")
                          plot(rsf)
                          # get distance matrix: rsf$proximity has dimension n x n.
                          # n = nRef + nNew
                          # Dimension distance matrix:
                          # rows: reference cases
                          # columns: new cases
                          if (self$refEQNew) {
                            self$distMat <- rsf$proximity
                          } else {
                            nRef <- nrow(self$refData)
                            self$distMat <- rsf$proximity[1:nRef, (nRef + 1):ncol(rsf$proximity)]
                          }
                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                        },
                        getSimilarCases = function(nCases) {
                          if (self$refEQNew) {
                            stop("no new data!")
                          }

                          start <- Sys.time()
                          cat("Start calculating similar cases...\n")
                          # learn if weights are empty
                          if (is.na(self$distMat))
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
                          sc <- simCases$new(distMat=self$distMat, method="rfProxy")
                          sc$getSimilarCases(self$newData, self$refData, self$learnVars, nCases)
                          self$orderMat <- sc$order
                          self$simCases <- sc$similarCases

                          end <- Sys.time()
                          duration <- round(as.numeric(end - start), 2)
                          cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                        }
                      ))