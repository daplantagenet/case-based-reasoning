#ifndef RANGERFOREST_H
#define RANGERFOREST_H

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>

#include <unordered_map>

typedef std::unordered_map<int, int> hashMap;
typedef std::unordered_map<int, arma::uvec> hashPaths;

class rangerForest {
public: 
  rangerForest() {};
  
  // get indices of terminal nodes
  arma::uvec terminalNodes(arma::mat& nodeIDs) {
    Rcpp::NumericVector ind;
    for (std::size_t i=0;i<nodeIDs.n_rows;++i) {
      if (nodeIDs.col(0)(i) == 0) {
        ind.push_back(i);
      }
    }
    return Rcpp::as<arma::uvec>(Rcpp::wrap(ind));
  };
  
  // transform matrix to hashmap
  hashMap nodeIdToHashMap(arma::umat& nodeIDs) {
    hashMap nodes;
    int nrow = nodeIDs.n_rows;
    for (std::size_t i=0;i<nrow;++i) {
      if (nodeIDs(i, 1) != 0) {
        nodes[nodeIDs(i, 1)] = nodeIDs(i, 0);
      }
      if (nodeIDs(i, 2) != 0) {
        nodes[nodeIDs(i, 2)] = nodeIDs(i, 0);
      }
    }
    return nodes;
  };
  
  // get the path to the root for length calculation
  arma::uvec pathToRoot(hashMap& nodes, int terminalNode) {
    Rcpp::NumericVector path;
    path.push_back(terminalNode);
    while (true) {
      // stop when at root
      if (terminalNode == 1) {
        break;
      }
      terminalNode = nodes.at(terminalNode - 1);
      path.push_back(terminalNode);
    }
    return Rcpp::as<arma::uvec>(Rcpp::wrap(path));
  };
  
  // calculate the number of edges between two terminal nodes
  int nodeDistance(arma::uvec& path1, arma::uvec& path2) {
    int n = path1.size();
    int m = path2.size();
    for (std::size_t i=0;i<n;++i) {
      for (std::size_t j=0;j<m;++j) {
        if (path1(i) == path2(j)) {
          return i + j;
        }
      }
    }
    // should not happen; at least root node is common node
    return -99;
  };
  
private:
  hashPaths hp_;
};

#endif
