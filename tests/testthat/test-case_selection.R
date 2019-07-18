testthat::context("CBRBase-Object")

testthat::test_that("Extract Similar Cases", {
  # set.seed(1234)
  # 
  # x <- iris[1:25, -5]
  # y <- iris[26:35, -5]
  # 
  # dist(iris[c(1, 26), -5], method = 'manhattan', p = 3)
  # CaseBasedReasoning::weightedDistance(as.matrix(iris[c(1, 26), -5]))
  # 
  # cc <- CaseBasedReasoning::weightedDistance(as.matrix(x), as.matrix(y))
  # cc
  # dist(as.matrix(x), as.matrix(y))
  # 
  # cbr <- CaseBasedReasoning:::CBRBase$new(a ~ b)
  # cbr$calc_distance_matrix
  # 
  # dist_mat <- matrix(c(1:10, 2:11, 3:12, 4:13, 5:14), 10, 5)
  # dist_mat %>% 
  #   as.matrix() %>% 
  #   cpp_orderMatrix(sortDirection = 0, k = 10) -> ord_matrix
  # 
  # colID <- 1:ncol(ord_matrix)
  # ord_matrix %>% 
  #   as_tibble() %>% 
  #   purrr::map2(.y = colID, 
  #               .f = function(rowIDs, colID, dtData, dist_mat) {
  #                 dtTmp <- dtData[rowIDs, ]
  #                 if (addDistance) {
  #                   dtTmp$scDist <- distanceMatrix[rowIDs, colID]
  #                 }
  #                 dtTmp
  #               }, dtData = dtData, dist_mat = dist_mat)
})