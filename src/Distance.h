#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "containers/RFDistanceContainer.h"

// typedef double (*funcPtr)(const arma::mat &X, const arma::mat &Y);

class Distance {
public:
  virtual ~Distance() {};
  virtual double calc_distance(const arma::rowvec& X, const arma::rowvec& Y) {return 0.0;};
  virtual std::string getClassName() { return "Distance";}
};

#endif
