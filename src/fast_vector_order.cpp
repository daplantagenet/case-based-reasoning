// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

//' Order distance vector
//'
//' @param mDist distance matrix
//' @param nCases number of cases
//' @return matrix with nearest cases
// [[Rcpp::export]]
Rcpp::NumericVector fast_Vector_Order(arma::vec vX, int n) {
  arma::uvec order = arma::sort_index(vX) + 1;
  order.resize(n);
  return(Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(order)));
}
