// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "ranger/RangerForestNodeDistance.h"

// [[Rcpp::export]]
Rcpp::DataFrame cpp_TerminalNodeDistance(arma::umat& nodeIDs) {
  RangerForestNodeDistance rf(nodeIDs);
  return rf.nodeDistance().asDataFrame();
}
