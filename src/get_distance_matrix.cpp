#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix get_Distance_Matrix(Rcpp::NumericMatrix queryData, Rcpp::NumericMatrix x, Rcpp::NumericVector weights) {
  int nVars = x.ncol();
  int nNewCases = queryData.nrow();
  Rcpp::NumericMatrix mResult(x.nrow(), nNewCases);
  for (int j=0; j<nNewCases;++j) {
    for (int i=0; i<nVars;++i) {
      mResult(Rcpp::_, j) = mResult(Rcpp::_, j) + abs(weights(i) * (x(Rcpp::_, i) - queryData(j, i)));
    }
  }
  return mResult;
}
