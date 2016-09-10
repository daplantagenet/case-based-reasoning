// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>


double proximityByObservation(arma::rowvec& x, arma::rowvec& y) {
  int nTrees = x.size();
  int similarity = 0;
  for (std::size_t k=0;k<nTrees;++k) {
    if (x(k) != y(k)) {
      ++similarity;
    }
  }
  return similarity * 1. / nTrees;
}


#if RCPP_PARALLEL_USE_TBB

// get the terminal for each observation
struct parallelTerminalNode : public RcppParallel::Worker {
  const arma::mat data_;
  const arma::vec childNodes1_;
  const arma::vec childNodes2_;
  const arma::vec splitValues_;
  const arma::vec splitVarIds_;
  arma::vec& output_;
  
  parallelTerminalNode(
    const arma::mat& data,
    const arma::vec childNodes1,
    const arma::vec childNodes2,
    const arma::vec splitValues,
    const arma::vec splitVarIds,
    arma::vec& output
  ) : data_(data), childNodes1_(childNodes1), childNodes2_(childNodes2), 
  splitValues_(splitValues), splitVarIds_(splitVarIds), output_(output)  {}
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin; i<end; ++i) {
      int nodeId = 1;
      double value = 0;
      while (true) {
        if ((childNodes1_(nodeId - 1) == 0 && childNodes2_(nodeId - 1) == 0)) {
          break;
        }
        int splitVarID = splitVarIds_(nodeId - 1);
        value = data_(i, splitVarID - 1);
        if (value <= splitValues_(nodeId - 1)) {
          nodeId = childNodes1_(nodeId - 1) + 1;
        } else {
          nodeId = childNodes2_(nodeId - 1) + 1;
        }
      }
      output_(i) = nodeId;
    }
  }
};

struct parallelProximityMatrix : public RcppParallel::Worker {
  const arma::mat data_;
  const int nrow_;
  arma::vec& output_;
  
  parallelProximityMatrix(
    const arma::mat& data,
    const int nrow,
    arma::vec& output
  ) : data_(data), nrow_(nrow), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      arma::rowvec x = data_.row(i);
      for (std::size_t j=i+1;j<nrow_;++j) {
        arma::rowvec y = data_.row(j);
        output_((2 * i * nrow_ - i * i + 2 * j - 3 * i - 2) / 2) = proximityByObservation(x, y);
      }
    }
  }
};

#else

// no single threated implementation

#endif

// [[Rcpp::export]]
arma::vec terminalNodeIDs(arma::mat& x,
                          arma::vec& childNodes1, 
                          arma::vec& childNodes2, 
                          arma::vec& splitValues, 
                          arma::vec& splitVarIds) {
  int nrow = x.n_rows;
  arma::vec output(nrow);
  output.fill(0);
  parallelTerminalNode parallelTerminalNode(x, childNodes1, childNodes2, splitValues, splitVarIds, output);
  parallelFor(0, nrow, parallelTerminalNode);
  return output;
}

// [[Rcpp::export]]
arma::vec proximityMatrix(arma::mat& x) {
  int nrow = x.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  output.fill(0);
  parallelProximityMatrix parallelProximityMatrix(x, nrow, output);
  parallelFor(0, nrow, parallelProximityMatrix);
  return output;
}
