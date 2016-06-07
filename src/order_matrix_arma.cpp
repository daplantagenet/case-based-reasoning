// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

#if RCPP_PARALLEL_USE_TBB

struct order_arma : public Worker {
  const arma::mat x;
  const int sortDirection;
  const int k;
  arma::umat& output;
  order_arma(const arma::mat x,
             const int sortDirection,
             const int k,
             arma::umat& output)
    : x(x), sortDirection(sortDirection), k(k), output(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    arma::uvec order(x.n_rows);
    for (std::size_t i = begin; i < end; ++i) {
      order = arma::sort_index(x.col(i), sortDirection);
      for (std::size_t l = 0; l < k; ++l) {
        output(l, i) = order(l) + 1;
      }
    }
  }
};

NumericMatrix get_order_matrix(arma::mat x, int sortDirection, int k) {
  int nCols = x.n_cols;
  arma::umat output(k, nCols);
  order_arma order_arma(x, sortDirection, k, output);
  parallelFor(0, nCols, order_arma);
  return(as<NumericMatrix>(wrap(output)));
}

#else

NumericMatrix get_order_matrix(arma::mat x, int sortDirection, int k) {
  int nCols = x.n_cols;
  arma::umat output(nCols, k);
  arma::uvec order(x.n_rows);
  for (std::size_t i; i<nCols; ++i) {
    order = arma::sort_index(x.col(i), sortDirection);
    for (std::size_t l = 0; l < k; ++l) {
      output(l, i) = order(l) + 1;
    }
  }
  return(as<NumericMatrix>(wrap(output)));
}

#endif

// [[Rcpp::export]]
NumericMatrix order_matrix(arma::mat x, int sortDirection = 0, int k = 5) {
  return(get_order_matrix(x, sortDirection, k));
}
