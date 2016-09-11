// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "ranger/rangerForest.hpp"

// [[Rcpp::export]]
arma::uvec getPath(arma::umat& nodeIDs, int k) {
  rangerForest rf;
  hashMap nodes = rf.nodeIdToHashMap(nodeIDs);
  return rf.pathToRoot(nodes, k);
}

// [[Rcpp::export]]
int distTest(arma::uvec& path1, arma::uvec& path2) {
  rangerForest rf;
  return rf.nodeDistance(path1, path2);
}
