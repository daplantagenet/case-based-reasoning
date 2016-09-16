// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List weighted_knn(arma::mat x,
                        arma::mat query,
                        arma::vec weights,
                        const int sortDirection = 0,
                        const int k = 1) {
  
  int nVars = weights.size();
  int nQuery = query.n_rows;
  
  arma::mat retDist(nQuery, k);
  arma::umat retOrder(nQuery, k);
  
  arma::colvec tmpDist(x.n_rows);
  tmpDist.fill(0);
  arma::uvec order(x.n_rows);
  
  for (auto i=0;i<nQuery;++i) {
    for (auto j=0;j <nVars;++j) {
      tmpDist = tmpDist + abs(weights(j) * (x.col(j) - query(i, j)));
    }
    order = arma::sort_index(tmpDist, sortDirection);
    for (std::size_t l=0; l<k;++l) {
      retDist(i, l) = tmpDist(order(l));
      retOrder(i, l) = order(l) + 1;
    }
    tmpDist.fill(0);
  }
  return Rcpp::List::create(
    Rcpp::Named("distance") = retDist,
    Rcpp::Named("order")    = retOrder
  );
}
