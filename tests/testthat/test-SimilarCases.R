testthat::context("Functionality: Similar Cases")

testthat::test_that("Survival Random Forest", {
  
})

testthat::test_that("Regression Random Forest", {
  
})

testthat::test_that("Classification Random Forest", {
  rf_model <- RFModel$new(Species ~ ., data=iris)
  iris %>% 
    rf_model$fit()
  testthat::expect_is(rf_model$model_fit, 'ranger')
  
  rf_model$set_distance_method('Depth')
  rf_model$calc_distance_matrix(iris[, -1]) %>% 
    as.matrix() -> d
})

testthat::test_that("Linear Regression", {
  
})

testthat::test_that("Logistic Regression", {
  
})

testthat::test_that("Cox-Proportional-Hazard", {
  
})