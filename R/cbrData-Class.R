cbrData <- R6Class("cbrData",
                   public = list(
                     formula   = NULL,
                     data      = NULL,
                     queryData = NULL,
                     distMat   = NULL,
                     orderMat  = NULL,
                     simCases  = NULL,
                     impute    = F,
                     # initialize class
                     initialize = function(formula, data, queryData=NULL, impute=F) {
                       formula <- formula(formula)
                       if (class(formula) != "formula") {
                         stop("Error: Invalid formula.")
                       }
                       self$formula <- formula
                       self$data <- as.data.table(data)
                       self$queryData <- as.data.table(queryData)
                       self$impute <- impute
                     },
                     # get query data, if there are missing values,
                     # return imputed data
                     get_query_data = function () {
                       return(self$queryData)
                     },
                     # get learning data, if it is imputed return imputed data
                     # else data.frame without missing cases
                     get_data = function () {
                       return(self$data)
                     },
                     get_sim_cases = function() {
                       return(self$simCases)
                     },
                     # return query + matched data
                     get_matched_data = function() {
                       if (is.null(self$simCases))
                         stop("Model is not learned")
                       
                       queryData <- self$queryData
                       queryData$group <- "Query Data"
                       matchedData <- self$simCases
                       matchedData$caseId <- NULL
                       matchedData$scDist <- NULL
                       matchedData$group <- "Matched Data"
                       return(rbind(queryData, simCases))
                     }
                   ),
                   private = list(
                     # check data sets
                     check_data = function(x, impute=F, isLearning=T) {
                       # drop cases with missing values in the relevant variables
                       if (!impute) {
                         x <- private$drop_missing(x, isLearning)
                         if (nrow(x) == 0) {
                           if (isLearning) {
                             stop("Learning data is empty after NA elimination")
                           } else {
                             stop("Query data is empty after NA elimination")
                           }
                         }
                       }

                       # check character variables: need factors
                       x <- private$check_factor(x)

                       # check levels of factor variables

                       # more tests
                       
                       return(x)
                     },
                     # drop missing values from data
                     drop_missing = function(x, isLearning=F) {
                       vars <- all.vars(self$formula)
                       rs <- rowSums(as.data.frame(is.na(x[, vars])))
                       idDrop <- which(rs > 0)
                       cat(paste0("Dropped cases with missing values: ", length(idDrop), "\n"))
                       if (isLearning) {
                         self$info$y[1] <- as.character(length(idDrop))
                       } else {
                         self$info$y[2] <- as.character(length(idDrop))
                       }
                       if (length(idDrop) > 0)
                         x <- x[-idDrop, ]
                       return(x)
                     },
                     # transform character variables to factor
                     check_factor = function(x) {
                       trf <- c()
                       for (var in self$learnVars) {
                         if (is.character(x[, var])) {
                           trf <- c(trf, var)
                           x[, var] <- factor(x[, var])
                         }
                       }
                       if (length(trf) > 0) {
                         cat(paste0("Following variables are transformed to class factor: ", paste(trf, collapse=", "), "\n"))
                       }
                       return(x)
                     },
                     # drop levels from each variable
                     drop_levels = function() {

                     },
                     # data preparation:
                     # we transform all factors to their corresponding
                     # weights and set weight equal to 1 for factor
                     # variables
                     transform_data = function(newCases, learning, learnVars, weights) {
                       nVars <- length(learnVars)
                       trafoweights <- rep(0, nVars)
                       for (j in 1:nVars) {
                         if (is.factor(learning[, learnVars[j]])) {
                           newCases[, learnVars[j]] <- weights[[learnVars[j]]][newCases[, learnVars[j]]]
                           learning[, learnVars[j]] <- weights[[learnVars[j]]][learning[, learnVars[j]]]
                           trafoweights[j] <- 1
                         } else { # else keep weights
                           trafoweights[j] <- weights[[learnVars[j]]]
                         }
                       }
                       names(trafoweights) <- NULL
                       return(list(newCases     = unname(as.matrix(newCases[, learnVars])),
                                   learning     = unname(as.matrix(learning[, learnVars])),
                                   trafoweights = trafoweights))
                     },
                     # calculate distance 
                     get_distance_matrix=function() {
                       # model specific
                     },
                     # get similar cases
                     get_similar_cases=function(k = 1) {
                       self$orderMat <- Similarity::orderCPP(as.matrix(self$distMat), k = k)
                       similarCases <- do.call(rbind, apply(self$orderMat, 1,
                                                            function(x, data=self$data) {
                                                              data[x, ]
                                                            }
                                                            )
                       )
                       # get distances
                       self$orderMat <- cbind(1:nrow(self$orderMat), self$orderMat)
                       distance <-  as.numeric(apply(self$orderMat, 1,
                                                     function(x, data=self$distMat) {
                                                       data[x[2:length(x)], x[1]]
                                                     }
                       ))
                       
                       # mark similar cases: 1:n ids
                       similarCases$caseId <- rep(1:nrow(queryData), each=nCases)
                       similarCases$scDist <- distance
                       self$similarCases <- similarCases
                     },
                     # calculate distance and return n nearest distance and
                     # row id of n nearest cases from reference data;
                     calc_kNN = function(newCases, learning, learnVars, weights, nCases) {
                       trfData <- private$transform_data(newCases, learning, learnVars, weights)
                       return(
                         cbr:::get_nearest_Elements(
                           x             = trfData$learning,
                           query         = trfData$newCases,
                           weights       = trfData$trafoweights,
                           sortDirection = 0L,
                           k             = nCases)
                       )
                     }
                   )
)
