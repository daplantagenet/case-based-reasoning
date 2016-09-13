#ifndef RFDEPTHDISTANCE_H
#define RFDEPTHDISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "distance.hpp"
#include "nodeDistContainer.hpp"

class rfDepthDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    double sum = 0.0;
    double d = 0.0;
    auto nTree = 0;
    for (auto t=0; t<nTrees_;++t) {
      if (x[t] < y[t]) {
        d = nodeDists_.getValue(x[t], y[t], t);
      } else if (x[t] > y[t]) {
        d = nodeDists_.getValue(y[t], x[t], t);
      } else {
        d = 0.0;
        sum += 0.0;
        ++nTree;
      }
      if (d > 0.0) {
        // TODO: set trafo
        sum += d;
        ++nTree;
      }
    }
    return sum * 1. / nTree;
  };
  
  void set_parameters(RfDistContainer nodeDist) {
    nodeDists_ = nodeDist;
    nTrees_ = nodeDist.getNTree();
  };
private:
  RfDistContainer nodeDists_;
  std::uint32_t nTrees_;
};

#endif
