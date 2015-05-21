// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List get_nearest_Elements(arma::mat mNew,
                                arma::mat mRef,
                                arma::vec vWeights,
                                int nCases) {
   try {
    // number of variables in model
    int nVars = vWeights.size();

    // n new cases
    int nNewCases = mNew.n_rows;

    // allocate distance and order matrix
    arma::mat retDist(nNewCases, nCases);
    arma::umat retOrder(nNewCases, nCases);

    // allocate tmp vector
    arma::colvec tmpDist(mRef.n_rows);
    arma::uvec order(mRef.n_rows);

    for (int i=0; i<nNewCases; ++i) { // loop over cases
      for (int j=0; j<nVars; ++j) { // loop over variables
        tmpDist = tmpDist + abs(vWeights(j) * (mRef.col(j) - mNew(i, j))); //
      } // end loop variables
      order = arma::sort_index(tmpDist);
      for (int k=0;k<nCases; ++k) {
        // write distance to final matrix
        retDist(i, k) = tmpDist(order(k));
        // write order to final matrix
        retOrder(i, k) = order(k) + 1;
      }
      // reset tmp distance vector
      tmpDist = arma::zeros<arma::vec>(mRef.n_rows);
    } // end loop cases
    return Rcpp::List::create(
      Rcpp::Named("distance") = retDist,
      Rcpp::Named("order")    = retOrder
    );
   } catch(std::exception &ex) {
     forward_exception_to_r(ex);
   } catch(...) {
     ::Rf_error("c++ exception (unknown reason)");
   }
   // never go here
   return NA_REAL;
}
