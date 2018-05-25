#ifndef DISTANCEFACTORY_H
#define DISTANCEFACTORY_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>
#include <memory>

#include "Utility.h"
#include "Distance.h"
#include "DistanceRF.h"
#include "ranger/RangerForestNodeDistance.h"
#include "containers/RFDistanceContainer.h"

class DistanceFactory : public Distance {
private: 
  std::shared_ptr<Distance> distance_;
public:
  explicit DistanceFactory(Rcpp::List& arguments) {
    using namespace utility;
    std::string distName = arguments["method"];
    if (isEqualStr(distName, "Proximity")) {
      int nTrees = 0;
      if (arguments.containsElementNamed("nTrees")) {
        nTrees = Rcpp::as<int>(arguments["nTrees"]);
      } else {
        Rcpp::stop("Parameter nTrees is neccessary for Proximity Distance.");
      }
      distance_ = std::make_shared<DistanceRFProximity>(nTrees);
    } else if (isEqualStr(distName, "Depth")) {
      // calculate terminal node edge length
      arma::umat terminalNodeIDs = Rcpp::as<arma::umat>(arguments["terminalNodeIDs"]);
      RangerForestNodeDistance rf(terminalNodeIDs);
      RfDistanceContainer nodeDists = rf.nodeDistance();
      distance_ = std::make_shared<Distance>();
      // distanceFunction = std::make_shared<DistanceRFDepth>();
    }
    distance_ = std::make_shared<Distance>();
  }
  virtual double calc_distance(const arma::rowvec& X, const arma::rowvec& Y);
  std::string getClassName() { return "DistanceFactory";}
};
#endif