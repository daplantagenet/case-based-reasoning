#define STRICT_R_HEADERS

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <memory>

#include "distance/weightedDistance.hpp"
#include "distance/rangerProximity.hpp"
#include "distance/rfDepthDistance.hpp"

// TODO: Representation of Results
// column-wise, row-wise, or full
#if RCPP_PARALLEL_USE_TBB

struct parallelDistance : public RcppParallel::Worker {
  const arma::mat input_;
  std::shared_ptr<distance> dist_;
  const int nrow_;
  arma::vec& output_;
  
  parallelDistance(
    const arma::mat& input,
    const std::shared_ptr<distance> dist,
    const int nrow,
    arma::vec& output
  ) : input_(input), dist_(dist), nrow_(nrow), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin; i<end; ++i) {
      arma::rowvec x = input_.row(i);
      for (std::size_t j=i + 1; j<nrow_; ++j) {
        arma::rowvec y = input_.row(j);
        output_((2 * i * nrow_ - i * i + 2 * j - 3 * i - 2) / 2) = dist_->calc_distance(x, y);
      }
    }
  }
};


arma::vec get_distance(arma::mat& input, std::shared_ptr<distance> dist) {
  int nrow = input.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  output.fill(0);
  parallelDistance parallelDistance(input, dist, nrow, output);
  parallelFor(0, nrow, parallelDistance);
  return output;
}

#else

// no single threated implementation

#endif


// [[Rcpp::export]]
arma::vec weightedDistanceCBR(arma::mat& x, arma::vec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  arma::vec ret = get_distance(x, std::make_shared<weightedDistance>(dist));
  return ret;
}


// // [[Rcpp::export]]
// arma::vec distanceRanger(arma::mat& x) {
//   // container with node IDs as keys and vector of number of trees as values
//   RfDistContainer rfDist(100);
//   
//   
//   // distance object 
//   // rfDepthDistance dist;
//   dist.set_parameters(rfDist);
//   return get_distance(x, std::make_shared<rfDepthDistance>(dist));
// }


// [[Rcpp::export]]
arma::vec proximityMatrixRangerCPP(arma::mat& x, std::uint32_t nTrees) {
  rangerProximity dist;
  dist.set_parameters(nTrees);
  return get_distance(x, std::make_shared<rangerProximity>(dist));
}
