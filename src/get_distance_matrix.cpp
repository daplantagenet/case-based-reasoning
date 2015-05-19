#include <Rcpp.h>

//' @export
// [[Rcpp::export]]
RcppExport SEXP get_Distance_Matrix(SEXP newData, SEXP refData, SEXP weights) {
  Rcpp::NumericVector vWeights(weights);
  Rcpp::NumericMatrix mNew(newData);
  Rcpp::NumericMatrix mRef(refData);

  // number of variables in model
  int nVars = mRef.ncol();
  // n new cases
  int nNewCases = mNew.nrow();

  // allocate result matrix
  Rcpp::NumericMatrix mResult(mRef.nrow(), nNewCases);

  for (int i=0; i<nVars; ++i) { // loop over variables
    for (int j=0; j<nNewCases; ++j) { // loop over new cases
      mResult(Rcpp::_, j) = mResult(Rcpp::_, j) + abs(vWeights(i) * (mRef(Rcpp::_, i) - mNew(j, i)));
    } // end loop new cases
  } // end loop variables

  return Rcpp::wrap(mResult);
}
