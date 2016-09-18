#include "distanceAPI.hpp"

void distanceAPI::init(arma::mat& x, std::string method, std::size_t p) {
  this->set_distance(method, p);
  this->calc(x);
};

void distanceAPI::set_distance(std::string distMethod, std::size_t p) {
  if (distMethod.compare("euclidian") == 0) {
    euclidianDistance dist;
    dist.set_parameters();
    this->dist_ = std::make_shared<euclidianDistance>(dist);
  } else if (distMethod.compare("manhattan") == 0) {
    manhattanDistance dist;
    dist.set_parameters();
    this->dist_ = std::make_shared<manhattanDistance>(dist);
  } else if (distMethod.compare("minkowski") == 0) {
    minkowskiDistance dist;
    dist.set_parameters(p);
    this->dist_ = std::make_shared<minkowskiDistance>(dist);
  }
  distance dist;
  this->dist_ = std::make_shared<distance>(dist);
};

void distanceAPI::calc(arma::mat& x) {
  int nrow = x.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  output_ = output;
  parallelDistance parallelDistance(x, this->dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistance);
};


/**
 * Weighted Distance Calculation
 */
void weightedDistanceAPI::init(arma::mat& x, arma::vec& weights) {
  this->set_distance(weights);
  this->calc(x);
}

void weightedDistanceAPI::set_distance(arma::vec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  this->dist_ = std::make_shared<weightedDistance>(dist);
};


/**
 * XY Distance Calculation
 */
void xyDistanceAPI::init(arma::mat& x, arma::mat& y, std::string method, std::size_t p) {
  this->set_distance(method, p);
  this->calc(x, y);
};

void xyDistanceAPI::calc(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(x, y, this->dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};


/**
 * Weighted XY Distance Calculation
 */
void weightedXYDistanceAPI::init(arma::mat& x, arma::mat& y, arma::vec& weights) {
  this->set_distance(weights);
  this->calc(x, y);
}

void weightedXYDistanceAPI::set_distance(arma::vec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  this->dist_ = std::make_shared<weightedDistance>(dist);
};


/**
 * RandomForests Terminal Node Distance
 */
void rfTerminalNodeDistanceAPI::init(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  output_ = rf.nodeDistance();
}


/**
 * RandomForests Proximity Matrix
 */
void rfProximityDistanceAPI::init(arma::mat& x) {
  this->set_distance(x);
  this->calc(x);
}

void rfProximityDistanceAPI::set_distance(arma::mat& x) {
  rangerProximity dist;
  dist.set_parameters(x.n_cols);
  this->dist_ = std::make_shared<rangerProximity>(dist);
}


/**
 * RandomForests XY Proximity Matrix
 */
void rfProximityXYDistanceAPI::init(arma::mat& x, arma::mat& y) {
  this->set_distance(x);
  this->calc(x, y);
};

void rfProximityXYDistanceAPI::calc(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(x, y, this->dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};


/**
 * RandomForests Depth Distance
 */
void rfDepthDistanceAPI::init(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
  // calculate terminal node edge length
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
  this->calc(xNodeIDs);
}

void rfDepthDistanceAPI::set_distance(RfDistContainer& nodeDists) {
  rfDepthDistance dist;
  dist.set_parameters(nodeDists);
  this->dist_ = std::make_shared<rfDepthDistance>(dist);
}

void rfDepthDistanceAPI::calc(arma::mat& xNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  output_ = output;
  parallelDistance parallelDistance(xNodeIDs, this->dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistance);
};


/**
 * RandomForests XY Depth Distance
 */
void rfDepthXYDistanceAPI::init(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  // calculate terminal node edge length
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
  this->calc(xNodeIDs, yNodeIDs);
}

void rfDepthXYDistanceAPI::calc(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::mat output(nrow, yNodeIDs.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(xNodeIDs, yNodeIDs, this->dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};
