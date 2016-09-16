#ifndef DISTANCEAPI_H
#define DISTANCEAPI_H

class distanceAPI {
public:
  distanceAPI(arma::arma::mat& x, std::string method = "euclidian", std::size_t p = 2) {
    this->set_distance(method, p);
    this->calc(x);
  }
  
  distanceAPI(arma::arma::mat& x, arma::vec& weights) {
    this->set_distance(method, weights);
    this->calc(x);
  }
  
  distanceAPI(arma::mat& x, arma::mat& y, std::string method = "euclidian", std::size_t p = 2) {
    this->set_distance(method, p);
    this->calc(x, y);
  }
  
  distanceAPI(arma::arma::mat& x, arma::vec& weights) {
    this->set_distance(method, weights);
    this->calc(x);
  }
  
  get() {
    
  }
  
private:
  void set_distance(std::string distMethod = "euclidian", std::size_t p = 2) {
    if (distMethod.compare("euclidian") == 0) {
      euclidianDistance dist;
      dist.set_parameters(x);
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
  }
  
  void set_distance(arma::vec& weights) {
    weightedDistance dist;
    dist.set_parameters(weights);
    this->dist_ = std::make_shared<weightedDistance>(dist);
  }
  
  void calc(arma::mat& x) {
    int nrow = x.n_rows;
    arma::vec output(nrow * (nrow - 1) / 2);
    output.fill(0);
    parallelDistance parallelDistance(x, this.dist_, nrow, output);
    parallelFor(0, nrow, parallelDistance);
    outputVec_ = output;
  }
  
  void calc(arma::mat& x, arma::mat& y) {
    int nrow = x.n_rows;
    int mrow = y.n_rows;
    arma::mat output(nrow, mrow);
    output.fill(0);
    parallelDistanceNM parallelDistanceNM(x, y, this->dist_, nrow, output);
    parallelFor(0, nrow, parallelDistanceNM);
    outputMat_ = output;
  }
  
  std::shared_ptr<distance> dist_;
  arma::vec outputVec_;
  arma::mat outputMat_;
};

#endif
