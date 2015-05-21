// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

//' Order distance matrix
//'
//' @param mDist distance matrix
//' @param nCases number of cases
//' @return matrix with nearest cases
// [[Rcpp::export]]
Rcpp::NumericMatrix fast_Matrix_Order(arma::mat mDist, int nCases) {
  try{
    // new cases are in columns
    int nNewCases = mDist.n_cols;

    // allocate order matrix
    arma::umat retOrder(nNewCases, nCases);

    // allocate tmp vector
    arma::uvec order(mDist.n_rows);

    // loop over new cases
    for (int i=0;i<nNewCases; ++i) {
      order = arma::sort_index(mDist.col(i));
      for (int k=0;k<nCases; ++k) {
        // write order to final matrix
        retOrder(i, k) = order(k) + 1;
      }
    }
    // returns a matrix with newcases in rows and nearest cases in columns
    return(Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(retOrder)));
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  // never go here
  return NA_REAL;
}
