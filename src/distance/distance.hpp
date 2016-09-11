#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

class distance {
public:
  distance() {};
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {return 0.0;};
  void set_parameters() {};
};

#endif
