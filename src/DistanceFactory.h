#ifndef DISTANCEFACTORY_H
#define DISTANCEFACTORY_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include <memory>

#include "Distance.h"
#include "ranger/RangerForestDistance.h"
#include "containers/RFDistanceContainer.h"

class DistanceFactory {
private: 
  arma::mat dataMatrix;
  std::vector<arma::mat> dataMatrixList;
  bool isDataMatrix;
public:
  explicit DistanceFactory(arma::mat &dataMatrix) : dataMatrix(dataMatrix), isDataMatrix(true) {};
  explicit DistanceFactory(std::vector<arma::mat> &dataMatrixList) : dataMatrixList(dataMatrixList), isDataMatrix(false) {};
  std::shared_ptr<Distance> createDistanceFunction(Rcpp::List& attr, Rcpp::List& arguments);
  arma::vec get() {return output_;};
};

#endif