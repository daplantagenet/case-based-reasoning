#ifndef WEIGHTEDDISTANCE_H
#define WEIGHTEDDISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "distance.hpp"

class weightedDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    double sum = 0.0;
    for (std::size_t k=0; k<iN_; ++k) {
      sum += std::abs(weights_(k) * (x(k) - y(k)));
    }
    return sum;
  };
  
  void set_parameters(arma::vec weights) {
    weights_ = weights;
    iN_ = weights_.size();
  };
  
private:
  arma::vec weights_;
  int iN_;
};

#endif
