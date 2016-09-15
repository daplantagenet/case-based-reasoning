#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "../simd/mapReducer.hpp"
#include "../containers/nodeDistContainer.hpp"

class distance {
public:
  distance() {};
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {return 0.0;};
  void set_parameters() {};
};


// weighted distance
class weightedDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return variadic::simdMapReduce(weightedL1DistReducer(), x, y, w_);;
  };
  
  void set_parameters(arma::vec weights) {
    weights_ = weights;
  };
private:
  arma::vec weights_;
};


// random forest proximity
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


// random forest depth distance
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
