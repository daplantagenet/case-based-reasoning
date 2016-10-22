cbrData <- R6Class("cbrData",
                   public = list(
                     formula   = NULL,
                     terms     = NULL,
                     endPoint  = NULL,
                     data      = NULL,
                     queryData = NULL,
                     distMat   = NULL,
                     orderMat  = NULL,
                     simCases  = NULL,
                     k         = NULL,
                     # initialize class
                     initialize = function(formula, data, queryData=NULL) {
                       formula <- formula(formula)
                       if (class(formula) != "formula") {
                         stop("Error: Invalid formula.")
                       }
                       self$formula <- formula
                       self$terms <- attr(terms(formula, data=self$data), which = "term.labels")
                       self$endPoint <- all.vars(formula)[1:2]
                       self$data <- as.data.table(data)
                       self$data <- private$check_data(self$data)
                       if (!is.null(queryData)) {
                         self$queryData <- as.data.table(queryData) 
                         self$queryData <- private$check_data(self$queryData, F) 
                       }
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
                         stop("Error: Model is not learned.")
                       
                       queryData <- self$queryData
                       queryData$scCaseId <- 1:nrow(queryData)
                       queryData$group <- "Query Data"
                       matchedData <- self$simCases
                       matchedData$scCaseId <- rep(1:nrow(queryData), each = self$k)
                       matchedData$caseId <- NULL
                       matchedData$scDist <- NULL
                       matchedData$group <- "Matched Data"
                       rbind(queryData, matchedData) %>% 
                         dplyr::arrange(scCaseId) %>% 
                         dplyr::select(-scCaseId)
                     },
                     validate_model = function(plot=T) {
                       if (is.null(self$simCases))
                         stop("Error: no similar cases.")
                       valSC <- cbrValidate$new()
                       return(valSC$validate(self$queryData,
                                             self$simCases,
                                             self$terms,
                                             plot))
                     },
                     # calculate distance matrix
                     calc_distance_matrix = function() {
                       # Start calculation
                       start <- Sys.time()
                       cat("Start calculating distance matrix...\n")
                       # get distance matrix
                       private$get_distance_matrix()
                       end <- Sys.time()
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                     },
                     # get similar cases from reference data
                     calc_similar_cases = function(k = 1) {
                       if (is.null(self$queryData)) {
                         stop("Error: no query data.")
                       }
                       start <- Sys.time()
                       cat("Start caclulating similar cases...\n")
                       
                       # check nCases input
                       if (!is.numeric(k))
                         stop("Error: k must be numeric.")
                       if (k <= 0)
                         stop("Error: k must be positive integer value.")
                       
                       self$k <- k
                       # catch floating numbers
                       k <- as.integer(k)
                       private$get_distance_matrix()
                       # calculate distance and order of cases based on distance calculation
                       private$get_similar_cases(k)
                       end <- Sys.time()
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                     }
                   ),
                   private = list(
                     # check data sets
                     check_data = function(x, isLearning=T) {
                       # drop cases with missing values in the relevant variables
                       x <- private$drop_missing(x, isLearning)
                       if (nrow(x) == 0) {
                         if (isLearning) {
                           stop("Error: Learning data is empty after NA elimination.")
                         } else {
                           stop("Error: Query data is empty after NA elimination.")
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
                       dtData <- x %>% 
                         dplyr::select_(.dots = c(self$endPoint, self$terms))
                       rs <- rowSums(is.na(dtData))
                       idDrop <- which(rs > 0)
                       cat(paste0("Dropped cases with missing values: ", length(idDrop), "\n"))
                       if (length(idDrop) > 0)
                         x <- x[-idDrop, ]
                       return(x)
                     },
                     # transform character variables to factor
                     check_factor = function(x) {
                       trf <- c()
                       for (var in self$terms) {
                         if (is.character(x[[var]])) {
                           trf <- c(trf, var)
                           x[[var]] <- factor(x[[var]])
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
                     #' transforms data to integer representation;
                     #' necessary for c++ functions
                     to_int = function(x) {
                       if (is.null(x))
                         return(x)
                       
                       for (i in 1:ncol(x)) {
                         if (is(x, "data.table")) {
                           x[[i]] <- as.numeric(as.factor(x[[i]]))
                         } else {
                           x[, i] <- as.numeric(as.factor(x[, i]))
                         }
                       }
                       return(x)
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
                       similarCases$caseId <- rep(1:nrow(self$queryData), each=k)
                       similarCases$scDist <- distance
                       self$simCases <- similarCases
                     },
                     # calculate distance and return n nearest distance and
                     # row id of n nearest cases from reference data;
                     calc_kNN = function(newCases, learning, learnVars, weights, k) {
                       trfData <- private$transform_data(newCases, learning, learnVars, weights)
                       return(
                         cbr:::get_nearest_Elements(
                           x             = trfData$learning,
                           query         = trfData$newCases,
                           weights       = trfData$trafoweights,
                           sortDirection = 0L,
                           k             = k)
                       )
                     }
                   )
)
