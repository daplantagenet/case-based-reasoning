#ifndef NODEDISTANCE_H
#define NODEDISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "distance.hpp"
#include "nodeDistContainer.hpp"

class nodeDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    double sum = 0.0;
    int nTree = 0;
    for (std::size_t t=0; t<nTrees_;++t) {
      if (x[t] < y[t]) {
        d = get_node_distance(treeMap_, x[t], y[t], t);
      } else if (x[t] > y[t]) {
        d = get_node_distance(treeMap_, y[t], x[t], t);
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
      return sum * 1. / nTree;
  };
  
  void set_parameters(RfDistContainer nodeDist) {
    nodeDists_ = nodeDist;
    nTrees_ = nodeDists_.getNTree();
  };
  
private:
  RfDistContainer nodeDists_;
  std::uint32_t nTrees_;
};

#endif
