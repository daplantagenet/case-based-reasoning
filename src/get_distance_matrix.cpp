#include <Rcpp.h>

// [[Rcpp::export]]
RcppExport SEXP get_Distance_Matrix(SEXP queryData, SEXP x, SEXP weights) {
  Rcpp::NumericVector vWeights(weights);
  Rcpp::NumericMatrix mNew(queryData);
  Rcpp::NumericMatrix mRef(x);
  int nVars = mRef.ncol();
  int nNewCases = mNew.nrow();
  Rcpp::NumericMatrix mResult(mRef.nrow(), nNewCases);
  for (int j=0; j<nNewCases; ++j) { // loop over new cases
    for (int i=0; i<nVars; ++i) { // loop over variables
      mResult(Rcpp::_, j) = mResult(Rcpp::_, j) + abs(vWeights(i) * (mRef(Rcpp::_, i) - mNew(j, i)));
    } // end loop new cases
  } // end loop variables
  return Rcpp::wrap(mResult);
}
