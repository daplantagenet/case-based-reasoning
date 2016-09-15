// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>

#if RCPP_PARALLEL_USE_TBB

// get the terminal for each observation
struct parallelTerminalNodes : public RcppParallel::Worker {
  const arma::mat data_;
  const arma::vec childNodes1_;
  const arma::vec childNodes2_;
  const arma::vec splitValues_;
  const arma::vec splitVarIds_;
  arma::vec& output_;
  
  parallelTerminalNodes(
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

#else

// no single threathed implementation

#endif

// [[Rcpp::export]]
arma::vec terminalNodeIDRanger(arma::mat& x,
                               arma::vec& childNodes1, 
                               arma::vec& childNodes2, 
                               arma::vec& splitValues, 
                               arma::vec& splitVarIds) {
  int nrow = x.n_rows;
  arma::vec output(nrow);
  output.fill(0);
  parallelTerminalNodes parallelTerminalNodes(x, childNodes1, childNodes2, splitValues, splitVarIds, output);
  parallelFor(0, nrow, parallelTerminalNodes);
  return output;
}

