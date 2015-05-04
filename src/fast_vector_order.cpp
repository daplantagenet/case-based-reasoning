// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
RcppExport SEXP fast_Vector_Order(SEXP x, SEXP n) {
  arma::vec vX = Rcpp::as<arma::vec>(x);
  int nSC = Rcpp::as<int>(n);
  arma::uvec order = arma::sort_index(vX) + 1;
  order.resize(nSC);
  return(Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(order)));
}
