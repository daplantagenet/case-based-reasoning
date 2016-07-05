// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::uvec order_vector(arma::vec x, int sort_direction = 0, int n = 0) {
  arma::uvec order = arma::sort_index(x, sort_direction) + 1;
  if (n > 0)
    order.resize(n);
  return(order);
}
