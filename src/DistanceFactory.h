#ifndef DISTANCEFACTORY_H
#define DISTANCEFACTORY_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include <memory>

#include "Distance.h"
#include "ranger/RangerForestNodeDistance.h"
#include "containers/RFDistanceContainer.h"

class DistanceFactory {
private: 
  arma::mat dataMatrix_;
  std::vector<arma::mat> dataMatrixList_;
  bool isDataMatrix_;
public:
  explicit DistanceFactory(arma::mat &dataMatrix) : dataMatrix_(dataMatrix), isDataMatrix_(true) {};
  explicit DistanceFactory(std::vector<arma::mat> &dataMatrixList) : dataMatrixList_(dataMatrixList), isDataMatrix_(false) {};
  std::shared_ptr<Distance> createDistanceFunction(Rcpp::List& attr, Rcpp::List& arguments);
};

#endif