// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::NumericVector fast_Vector_Order(arma::vec vX, int n) {
  arma::uvec order = arma::sort_index(vX) + 1;
  order.resize(n);
  return(Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(order)));
}
