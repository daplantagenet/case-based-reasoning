#ifndef RANGERPROXIMITY_H
#define RANGERPROXIMITY_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "distance.hpp"
#include "nodeDistContainer.hpp"

class rangerProximity : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    std:: int32_t similarity = 0;
    for (auto k=0;k<nTrees_;++k) {
      if (x(k) != y(k)) {
        ++similarity;
      }
    }
    return similarity * 1. / nTrees_;
  };
  
  void set_parameters(std::uint32_t nTrees) {
    nTrees_ = nTrees;
  };
  
private:
  std::uint32_t nTrees_;
};
  
#endif
  
