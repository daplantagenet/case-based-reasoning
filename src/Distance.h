#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "containers/RFDistanceContainer.h"

class Distance {
public:
  Distance() {};
  virtual double calc_distance(const arma::mat& X, const arma::mat& Y) {return 0.0;};
  
  void set_parameters() {};
};

#endif
