// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distanceAPI.h"

<<<<<<< HEAD
/**
* Standard distance functions
*/
// [[Rcpp::export]]
arma::vec distanceCPP(arma::mat& x, std::string method="euclidian", int p = 2) {
  distanceAPI dist;
  dist.init(x, method, p);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat distanceXYCPP(arma::mat& x, arma::mat& y, std::string method="euclidian", int p = 2) {
  xyDistanceAPI dist;
  dist.init(x, y, method, p);
  return dist.get();
}

// [[Rcpp::export]]
arma::vec wDistanceCPP(arma::mat& x, arma::rowvec& weights) {
=======
// [[Rcpp::export]]
arma::vec cpp_weightedDistance(arma::mat& x, arma::rowvec& weights) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  weightedDistanceAPI dist;
  dist.init(x, weights);
  return dist.get();
}

// [[Rcpp::export]]
<<<<<<< HEAD
arma::mat wDistanceXYCPP(arma::mat& x, arma::mat& y, arma::rowvec& weights) {
=======
arma::mat cpp_weightedDistanceXY(arma::mat& x, arma::mat& y, arma::rowvec& weights) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  weightedXYDistanceAPI dist;
  dist.init(x, y, weights);
  return dist.get();
}

/**
 * Ranger RandomForest related distances
 */
// [[Rcpp::export]]
<<<<<<< HEAD
Rcpp::DataFrame terminalNodeDistanceCPP(arma::umat& terminalNodeIDs) {
=======
Rcpp::DataFrame cpp_TerminalNodeDistance(arma::umat& terminalNodeIDs) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  rfTerminalNodeDistanceAPI dist;
  dist.init(terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
<<<<<<< HEAD
arma::vec proximityMatrixRangerCPP(arma::mat& nodeIDs) {
=======
arma::vec cpp_proximityMatrix(arma::mat& nodeIDs) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  rfProximityDistanceAPI dist;
  dist.init(nodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
<<<<<<< HEAD
arma::mat proximityMatrixRangerCPPNM(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
=======
arma::mat cpp_proximityMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  rfProximityXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
<<<<<<< HEAD
arma::vec depthMatrixRangerCPP(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
=======
arma::vec cpp_depthMatrix(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
  rfDepthDistanceAPI dist;
  dist.init(xNodeIDs, terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
<<<<<<< HEAD
arma::mat depthMatrixRangerCPPXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs, terminalNodeIDs);
  return dist.get();
}
=======
arma::mat cpp_depthMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs, terminalNodeIDs);
  return dist.get();
}
>>>>>>> 87ba9a42a639891864e0592dbe1166751248c06d
