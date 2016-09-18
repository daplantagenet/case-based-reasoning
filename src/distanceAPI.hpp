#ifndef DISTANCEAPI_H
#define DISTANCEAPI_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distance/distance.hpp"
#include "parallelFrameworks.hpp"

#include "ranger/rangerForest.hpp"
#include "containers/nodeDistContainer.hpp"

/**
 * Distance Calculation
 */
class distanceAPI {
public:
  distanceAPI() {};
  void init(arma::mat& x, std::string method = "euclidian", std::size_t p = 2);
  arma::vec get() {return output_;};
  
protected:
  void set_distance(std::string distMethod = "euclidian", std::size_t p = 2);
  void calc(arma::mat& x);

  std::shared_ptr<distance> dist_;
  arma::vec output_;
};


/**
 * Weighted Distance Calculation
 */
class weightedDistanceAPI : public distanceAPI {
public:
  void init(arma::mat& x, arma::vec& weights);
  
protected:
  virtual void set_distance(arma::vec& weights);
};


/**
 * XY Distance Calculation
 */
class xyDistanceAPI : public distanceAPI {
public:
  void init(arma::mat& x, arma::mat& y, std::string method = "euclidian", std::size_t p = 2);
  arma::mat get() {return output_;};
  
protected:
  virtual void calc(arma::mat& x, arma::mat& y);
  arma::mat output_;
};


/**
 * Weighted XY Distance Calculation
 */
class weightedXYDistanceAPI : public xyDistanceAPI {
public:
  void init(arma::mat& x, arma::mat& y, arma::vec& weights);
  arma::mat get() {return output_;};
  
protected:
  virtual void set_distance(arma::vec& weights);
  
  arma::mat output_;
};


/**
 * RandomForests Terminal Node Distance
 */
class rfTerminalNodeDistanceAPI : public distanceAPI {
public:
  void init(arma::umat& nodeIDs);
  Rcpp::DataFrame get() {return output_.asDataFrame();};
  
protected:
  void set_distance() {};
  void calc() {};
  
  RfDistContainer output_;
};


/**
 * RandomForests Proximity Matrix
 */
class rfProximityDistanceAPI : public distanceAPI {
public:
  void init(arma::mat& x);
  
protected:
  void set_distance(arma::mat& x);
};

/**
 * RandomForests XY Proximity Matrix
 */
class rfProximityXYDistanceAPI : public rfProximityDistanceAPI {
public:
  void init(arma::mat& x, arma::mat& y);
  arma::mat get() {return output_;};
  
protected:
  virtual void calc(arma::mat& x, arma::mat& y);
  
  arma::mat output_;
};


/**
 * RandomForests Depth Distance Calculation
 */
class rfDepthDistanceAPI : public distanceAPI {
public:
  void init(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs);
  
protected:
  void set_distance(RfDistContainer& nodeDists);
  virtual void calc(arma::mat& xNodeIDs);
};


/**
 * RandomForests XY Depth Distance Calculation
 */
class rfDepthXYDistanceAPI : public rfDepthDistanceAPI {
public:
  void init(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs);
  arma::mat get() {return output_;};
  
protected:
  virtual void calc(arma::mat& xNodeIDs, arma::mat& yNodeIDs);
  
  arma::mat output_;
};

#endif
