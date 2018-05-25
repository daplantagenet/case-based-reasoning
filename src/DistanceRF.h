#ifndef DistanceRF_H
#define DistanceRF_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "Distance.h"
#include "containers/RfDistanceContainer.h"

//-------------------------------------
// Random Forest Proximity Distance
//-------------------------------------
class DistanceRFProximity : public Distance {
private:
  int nTrees_;
public:
  explicit DistanceRFProximity(int nTrees) {
    nTrees_ = nTrees;
  }
  virtual double calc_Distance(const arma::rowvec& X, const arma::rowvec& Y) {
    unsigned int n = X.n_cols;
    int similarity = 0;
    for (std::size_t i=0;i<n;++i) {
      if (X(i, 0) == Y(i, 0)) {
        ++similarity;
      }
    }
    Rcpp::Rcout << "sim: " << similarity << std::endl;
    return similarity * 1. / nTrees_;
  };
  std::string getClassName() { return "DistanceRFProximity";}
};


//-------------------------------------
// Random Forest Depth Distance
//-------------------------------------
class DistanceRFDepth : public Distance {
private:
  RfDistanceContainer nodeDists_;
  std::uint32_t nTrees_;
public:
  explicit DistanceRFDepth(RfDistanceContainer nodeDist) {
    nodeDists_ = nodeDist;
    nTrees_ = nodeDist.getNTree();
  }
  virtual double calc_Distance(const arma::rowvec& X, const arma::rowvec& Y) {
    double sum = 0.0;
    double d = 0.0;
    std::size_t nTree = 0;
    for (std::size_t t=0; t<nTrees_;++t) {
      if (X(t, 0) < Y(t, 0)) {
        d = nodeDists_.getValue(X(t, 0), Y(t, 0), t);
      } else if (X(t, 0) > Y(t, 0)) {
        d = nodeDists_.getValue(Y(t, 0), X(t, 0), t);
      } else {
        d = 0.0;
        ++nTree;
      }
      if (d > 0.0) {
        sum += d;
        ++nTree;
      }
    }
    return sum;
  };
};
#endif