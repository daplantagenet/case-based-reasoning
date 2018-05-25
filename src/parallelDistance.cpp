#define STRICT_R_HEADERS

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "DistanceRF.h"
#include "DistanceFactory.h"

#include <memory> 

using namespace Rcpp;
using namespace RcppParallel;

unsigned long matToVecIdx(const unsigned long i, const unsigned long j, const unsigned long N) {
  return (2 * i * N - i * i + 2 * j - 3 * i - 2) / 2;
}

struct ParallelDistanceVec : public Worker {
  const arma::mat input_;
  std::shared_ptr<Distance> distance_;
  std::size_t nrow_ = 0;
  Rcpp::NumericVector& output_;
  
  ParallelDistanceVec(const arma::mat input, 
                      std::shared_ptr<Distance> distance, 
                      Rcpp::NumericVector& output) 
    : input_(input), distance_(distance), output_(output) {
    nrow_ = input_.n_rows;
    Rcpp::Rcout << distance_->getClassName() << std::endl;
    Rcpp::Rcout << nrow_ << std::endl;
  }
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      for (std::size_t j=i+1;j<nrow_;++j) {
        output_[matToVecIdx(i, j, nrow_)] = distance_->calc_distance(input_.row(i), input_.row(j));
      }
    }
  }
};


// [[Rcpp::export]]
Rcpp::NumericVector cpp_parallelDistance(arma::mat& x, Rcpp::List arguments) {
  DistanceFactory distanceFunction(arguments);
  Rcpp::Rcout << distanceFunction.getClassName() << std::endl;
  
  // output
  unsigned long nrow = x.n_rows;
  Rcpp::NumericVector output(nrow * (nrow - 1) / 2);
  
  // perform distance calculation
  ParallelDistanceVec parallelDistanceVec(x, std::make_shared<DistanceFactory>(distanceFunction), output);
  parallelFor(0, nrow, parallelDistanceVec);
  
  return output;
}


struct ParallelDistanceVecXY : public Worker {
  const arma::mat x_;
  const arma::mat y_;
  std::shared_ptr<Distance> distance_;
  std::size_t nrow_ = 0;
  Rcpp::NumericMatrix& output_;
  
  ParallelDistanceVecXY(const arma::mat x, 
                        const arma::mat y, 
                        std::shared_ptr<Distance> distance, 
                        Rcpp::NumericMatrix& output) 
    : x_(x), y_(y), distance_(distance), output_(output) {
    nrow_ = y_.n_rows;
  }
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      for (std::size_t j=0;j<nrow_;++j) {
        output_[i, j] = distance_->calc_distance(x_.row(i), y_.row(j));
      }
    }
  }
};


// [[Rcpp::export]]
Rcpp::NumericVector cpp_parallelDistanceXY(arma::mat& x, arma::mat& y, Rcpp::List arguments) {
  DistanceFactory distanceFunction(arguments);
  Rcpp::Rcout << distanceFunction.getClassName() << std::endl;
  
  // output
  unsigned long nrowX = x.n_rows;
  unsigned long nrowY = y.n_rows;
  Rcpp::NumericMatrix output(nrowX, nrowY);
  
  // perform distance calculation
  ParallelDistanceVecXY parallelDistanceVecXY(x, y, std::make_shared<DistanceFactory>(distanceFunction), output);
  parallelFor(0, nrowX, parallelDistanceVecXY);
  
  return output;
}
