// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "ranger/rangerForest.hpp"
#include "containers/nodeDistContainer.hpp"

// [[Rcpp::export]]
Rcpp::DataFrame terminalNodeDistanceCPP(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  return nodeDists.asDataFrame();
}
