# library(survival)
# library(cbr)
#
# ovarian$tt <- factor(sample(1:3, 26, replace=T))
# sc <- cbrCoxModel$new(ovarian, endPoint=c("futime", "fustat"))
# sc$getFullDistanceMatrix(ovarian)
# sc$distMat
# sc$getSimilarCases(ovarian, 3)
#
# ovarian$tt <- factor(sample(1:3, 26, replace = T))
# test <- ovarian[rep(1:26, each=1000), ]
# sc <- cbrCoxModel$new(test, endPoint=c("futime", "fustat"))
# sc$getFullDistanceMatrix(test)
# dim(sc$distMat)
# ss <- sc$getSimilarCases(test, 1)
#
# a <- matrix(1:20, 10, 2)
# b <- matrix(1:18, 9, 2)
# w <- c(1, 1)
# .Call("cbr_get_Distance_Matrix", a, b, w,  PACKAGE = "cbr")
# .Call("cbr_get_nearest_Elements", a, b, w, 2,  PACKAGE = "cbr")
# .Call("cbr_fast_Vector_Order", 1:10, 5, PACKAGE = "cbr")
#
# nearestElements(a, b, w, 2)
#
