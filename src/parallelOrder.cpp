// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

struct ParallelOrderMatrix : public Worker {
  const arma::mat& x_;
  const int sortDirection_;
  const std::size_t k_;
  arma::umat& output_;
  
  ParallelOrderMatrix(
    const arma::mat& x,
    const int sortDirection,
    const std::size_t k,
    arma::umat& output
  ) : x_(x), sortDirection_(sortDirection), k_(k), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    arma::uvec order(x_.n_rows);
    for (std::size_t i=begin;i<end;++i) {
      order = arma::sort_index(x_.col(i), sortDirection_);
      for (std::size_t l=0;l<k_;++l) {
        output_(l, i) = order(l) + 1;
      }
    }
  }
};

arma::umat orderMatrix(arma::mat& X, const int sortDirection, const int k) {
  int nCols = X.n_cols;
  int nRows = k;
  if (k == 0) {
    nRows = X.n_rows;
  } 
  arma::umat output(nRows, nCols);
  output.fill(0);
  ParallelOrderMatrix parallelOrderMatrix(X, sortDirection, nRows, output);
  parallelFor(0, nCols, parallelOrderMatrix);
  return output;
}

// [[Rcpp::export]]
arma::umat cpp_order_matrix(arma::mat& x, const int sortDirection, int k = 5) {
  return orderMatrix(x, sortDirection, k);
}

// [[Rcpp::export]]
arma::uvec cpp_order_vector(arma::vec x, const int sortDirection, int k = 0) {
  arma::uvec order = arma::sort_index(x, sortDirection) + 1;
  if (k > 0)
    order.resize(k);
  return order;
}