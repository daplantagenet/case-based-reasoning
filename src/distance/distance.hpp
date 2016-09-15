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


// random forest proximity
class rangerProximity : public distance {
public:
  virtual double calc_distance(arma::rowvec& x, arma::rowvec& y) const {
    std::int32_t similarity = arma::accu(x != y);
    return similarity * 1. / nTrees_;
  };
  
  void set_parameters(std::uint32_t nTrees) {
    nTrees_ = nTrees;
  };
private:
  std::uint32_t nTrees_;
};


#endif
