// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "ranger/rangerForest.hpp"
#include "containers/nodeDistContainer.hpp"
#include "distance/distance.hpp"

// [[Rcpp::export]]
arma::vec proximityMatrixRangerCPP(arma::mat& x, std::uint32_t nTrees) {
  rangerProximity dist;
  dist.set_parameters(nTrees);
  return get_distance(x, std::make_shared<rangerProximity>(dist));
}

// [[Rcpp::export]]
arma::vec proximityMatrixRangerCPPNM(arma::mat& x, arma::mat& y, std::uint32_t nTrees) {
  rangerProximity dist;
  dist.set_parameters(nTrees);
  return get_distance(x, std::make_shared<rangerProximity>(dist));
}

// [[Rcpp::export]]
arma::vec depthMatrixRangerCPP(arma::mat& x, arma::umat& terminalNodeIDs) {
  // calculate terminal node edge length
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  // setup rf depth distance container
  rfDepthDistance dist;
  dist.set_parameters(nodeDists);
  // calculate distance
  return get_distance(x, std::make_shared<rfDepthDistance>(dist));
}

// [[Rcpp::export]]
Rcpp::DataFrame terminalNodeDistanceCPP(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  return nodeDists.asDataFrame();
}
