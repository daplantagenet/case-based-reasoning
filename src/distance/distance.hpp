#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "../containers/nodeDistContainer.hpp"

class distance {
public:
  distance() {};
  
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return 0.0;
  };
  
  void set_parameters() {};
};

// weighted distance
class weightedDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return arma::accu(weights_ % (x - y));
  };
  
  void set_parameters(arma::rowvec weights) {
    weights_ = weights;
  };
private:
  arma::rowvec weights_;
};

// euclidian distance
class euclidianDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return std::sqrt(arma::accu(arma::square((x - y))));
  };
  
  void set_parameters() {
  };
};

// minkowski distance
class minkowskiDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return arma::accu(arma::pow(x - y, p_));
  };
  
  void set_parameters(std::size_t p) {
    p_ = p;
  };
private:
  std::size_t p_;
};

// manhattan distance
class manhattanDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return arma::accu(arma::abs(x - y));
  };
  void set_parameters() {
  };
};

// maximum distance
class maximumDistance : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return arma::max(arma::abs(x - y));
  };
  void set_parameters() {
  };
};

// random forest proximity
class rangerProximity : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    return arma::accu(x != y) * 1 / nTrees_;
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
      }
      if (d >= 0.0) {
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
