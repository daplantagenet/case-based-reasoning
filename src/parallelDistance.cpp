#define STRICT_R_HEADERS

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "Distance.h"
#include "DistanceFactory.h"

#include <memory> 

using namespace Rcpp;
using namespace RcppParallel;

unsigned long matToVecIdx(const unsigned long i, const unsigned long j, const unsigned long N) {
  return (2 * i * N - i * i + 2 * j - 3 * i - 2) / 2;
}

struct ParallelDistanceVec : public Worker {
  std::vector<arma::mat> input_;
  std::shared_ptr<Distance> distance_;
  std::size_t nrow_ = 0;
  Rcpp::NumericVector& output_;
  
  ParallelDistanceVec(std::vector<arma::mat> input, std::shared_ptr<Distance>& distance, Rcpp::NumericVector& output) 
    : input_(input), distance_(distance), output_(output) {
    nrow_ = input_.at(0).n_rows;
  }
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      for (std::size_t j=i+1;j<nrow_;++j) {
        output_[matToVecIdx(i, j, nrow_)] = distance_->calc_distance(input_.at(0).row(i), input_.at(1).row(j));
      }
    }
  }
};


// [[Rcpp::export]]
Rcpp::NumericVector cpp_parallelDistance(Rcpp::List dataList, Rcpp::List attrs, Rcpp::List arguments) {
  // Convert list to vector of double matrices
  std::vector<arma::mat> listVec;
  for (List::iterator it = dataList.begin(); it != dataList.end(); ++it) {
    listVec.push_back(as<arma::mat >(*it));
  }
  std::shared_ptr<Distance> distanceFunction = DistanceFactory(listVec).createDistanceFunction(attrs, arguments);
  
  // output
  unsigned long nrow = listVec.at(0).n_rows;
  Rcpp::NumericVector output(nrow * (nrow - 1) / 2);
  
  // perform distance calculation
  ParallelDistanceVec* distanceWorker = new ParallelDistanceVec(listVec, distanceFunction, output);
  parallelFor(0, nrow, (*distanceWorker));
  delete distanceWorker;
  distanceWorker = NULL;
  
  return output;
}
