// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distanceApi.hpp"

/**
 * Standard distance functions
 */

/**
 * Ranger RandomForest related distances
 */
// [[Rcpp::export]]
Rcpp::DataFrame terminalNodeDistanceCPP(arma::umat& terminalNodeIDs) {
  rfTerminalNodeDistanceAPI dist;
  dist.init(terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::vec proximityMatrixRangerCPP(arma::mat& nodeIDs) {
  rfProximityDistanceAPI dist;
  dist.init(nodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat proximityMatrixRangerCPPNM(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  rfProximityXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::vec depthMatrixRangerCPP(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthDistanceAPI dist;
  dist.init(xNodeIDs, terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat depthMatrixRangerCPPXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs, terminalNodeIDs);
  return dist.get();
}
