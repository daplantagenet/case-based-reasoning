// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "distance/distance.hpp"

// [[Rcpp::export]]
arma::vec weightedDistanceCPP(arma::mat& x, arma::vec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  arma::vec ret = get_distance(x, std::make_shared<weightedDistance>(dist));
  return ret;
}

// [[Rcpp::export]]
arma::vec weightedDistanceCPPNM(arma::mat& x, arma::mat& y, arma::vec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  arma::vec ret = get_distanceNM(x, y, std::make_shared<weightedDistance>(dist));
  return ret;
}
