cbrData <- R6Class("cbrData",
                   public = list(
                     refData   = NA,
                     newData   = NA,
                     learnVars = NA,
                     endPoint  = NA,
                     refEQNew  = FALSE,
                     impute    = FALSE,
                     # initialize class
                     initialize = function(refData, newData, learnVars, endPoint, impute=FALSE) {
                       # check for missing input
                       if (missing(refData)) {
                         stop("Please add reference data!")
                       }

                       # check endpoint
                       if (missing(endPoint)) {
                         self$endPoint <- c("Time2Event", "Event")
                       } else {
                         if (length(endPoint) != 2)
                           stop("End point variable need to be of length 2!")
                         self$endPoint <- endPoint
                       }
                       chkEP <- self$endPoint %in% names(refData)
                       if (sum(chkEP) != 2)
                         stop("End point is not in reference data!")

                       # are there variables for learning
                       if (missing(learnVars)) {
                         cat("All variables of the reference data will be used for learning!\n")
                         idEP <- which(names(refData) %in% self$endPoint)
                         self$learnVars <- names(refData)[-idEP]
                       } else {
                         self$learnVars <- learnVars
                       }

                       # check data & add data to internal frame
                       self$refData <- private$check_data(refData, impute)

                       # validation check new data
                       if (missing(newData)) {
                         cat("No new data: use reference data for distance calculation!\n")
                         self$newData <- self$refData
                         self$refEQNew <- TRUE
                       } else {
                         # validation check new data
                         self$newData <- private$check_data(newData, impute, isReference=F)
                       }

                       # impute data; just RF
                       self$impute <- impute

                     }
                   ),
                   private = list(
                     refDataValid = FALSE,
                     newDataValid = FALSE,
                     # check data sets
                     check_data = function(x, impute=F, isReference=T) {
                       # check variable names new data are in data
                       inData <- !(self$learnVars %in% names(x))
                       if (any(inData)) {
                         if (isReference) {
                           private$refDataValid <- FALSE
                         } else {
                           private$newDataValid <- FALSE
                         }
                         missingVars <- self$learnVars[which(inData)]
                         stop(paste0("Following learning variables are missing: ", paste(missingVars, collapse=",")))
                       } else {
                         if (isReference) {
                           private$refDataValid <- TRUE
                         } else {
                           private$newDataValid <- TRUE
                         }
                       }

                       # if isReference data : check for Time2Event and Event variables
                       if (isReference) {
                         if(any(!self$endPoint %in% names(x))) {
                           self$endPoint <- NA
                           private$refDataValid <- FALSE
                           stop("Endpoint variables are not in reference data")
                         }
                       }

                       # drop cases with missing values in the relevant variables
                       if (!impute) {
                         x <- private$drop_missing(x)
                         if (nrow(x) == 0) {
                           if (isReference) {
                             stop("Reference data is empty after NA elimination")
                           } else {
                             stop("Data is empty after NA elimination")
                           }
                         }
                       }

                       # check character variables: need factors
                       x <- private$check_factor(x)

                       # check levels of factor variables

                       # more tests
                     },
                     # drop missing values from data
                     drop_missing = function(x) {
                       rs <- rowSums(is.na(x[, self$learnVars]))
                       idDrop <- which(rs > 0)
                       cat(paste0("Dropped cases with missing values: ", length(idDrop), "\n"))
                       if (length(idDrop) > 0)
                         x <- x[-idDrop, ]

                       return(x)
                     },
                     # transform character variables to factor
                     check_factor = function(x) {
                       trf <- c()
                       for (var in self$learningVars) {
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