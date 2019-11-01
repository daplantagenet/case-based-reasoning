testthat::context("Order")

testthat::test_that("Vector ordering", {
  set.seed(1234)
  x <- sample(1:20, size = 20)
<<<<<<< HEAD
  xOrder <- as.numeric(orderVectorCPP(x, 0))
=======
  xOrder <- cpp_orderVector(x)
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  yOrder <- order(x)
  testthat::expect_equal(xOrder, yOrder)
})

testthat::test_that("Matrix ordering", {
  set.seed((1234))
  x <- matrix(rnorm(100), 10)
  xOrder <- cpp_orderMatrix(x, sortDirection = 0, k = 10)
  yOrder <- do.call(cbind, lapply(1:nrow(x), function(col) {order(x[, col])}))
  testthat::expect_equal(xOrder, yOrder)
})