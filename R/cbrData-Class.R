cbrData <- R6Class("cbrData",
                   public = list(
                     learning  = NA,
                     queryData = NA,
                     learnVars = NA,
                     endPoint  = NA,
                     refEQNew  = FALSE,
                     impute    = FALSE,
                     warnings  = "",
                     info      = data.frame(x=c("Dropped cases with missing values in learning set:",
                                                "Dropped cases with missing values in query set:",
                                                "Imputation:",
                                                "Variablen:",
                                                "Variablen nach Selektion:"), 
                                            y=NA),
                     # initialize class
                     initialize = function(learning, queryData, learnVars, endPoint, impute=FALSE) {
                       # check for missing input
                       if (missing(learning)) {
                         stop("Please add data for learning the algorithm!")
                       }

                       # check endpoint
                       if (missing(endPoint)) {
                         stop("Please add endpoint!")
                       } 
                       chkEP <- self$endPoint %in% names(learning)
                       if (sum(chkEP) > 0)
                         stop("Endpoint is not in data!")

                       # are there variables for learning
                       if (missing(learnVars)) {
                         cat("All variables of the queryData data will be used for learning!\n")
                         idEP <- which(names(learning) %in% self$endPoint)
                         self$learnVars <- names(learning)[-idEP]
                       } else {
                         self$learnVars <- learnVars
                       }
          
                       if (missing(impute))
                         self$impute <- FALSE
                       
                       self$info$y[3] <- as.character(self$impute)
                       self$info$y[5] <- "keine Variablenselektion durchgeführt!"
                       
                       # check data & add data to internal frame
                       self$learning <- private$check_data(as.data.frame(learning), impute)

                       # validation check query data
                       if (missing(queryData)) {
                         cat("No query data: use reference data for distance calculation!\n")
                         self$queryData <- self$learning
                         self$refEQNew <- TRUE
                       } else {
                         # validation check query data
                         self$queryData <- private$check_data(as.data.frame(queryData), impute, isLearning=F)
                       }
                       # impute data; just RSF
                       self$impute <- impute
                     }
                   ),
                   private = list(
                     learningValid = FALSE,
                     queryDataValid = FALSE,
                     # check data sets
                     check_data = function(x, impute=F, isLearning=T) {
                       # check variable names verum data are in data
                       inData <- !(self$learnVars %in% names(x))
                       if (any(inData)) {
                         if (isLearning) {
                           private$learningValid <- FALSE
                         } else {
                           private$queryDataValid <- FALSE
                         }
                         missingVars <- self$learnVars[which(inData)]
                         stop(paste0("Following learning variables are missing: ", paste(missingVars, collapse=",")))
                       } else {
                         if (isLearning) {
                           private$learningValid <- TRUE
                         } else {
                           private$queryDataValid <- TRUE
                         }
                       }

                       # if isLearning data
                       if (isLearning) {
                         if(any(!self$endPoint %in% names(x))) {
                           self$endPoint <- NA
                           private$learningValid <- FALSE
                           stop("Endpoint variables are not in learning data")
                         }
                       }

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
                       rs <- rowSums(as.data.frame(is.na(x[, self$learnVars])))
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

                     }
                   )
)
