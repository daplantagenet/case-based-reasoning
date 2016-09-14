// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "ranger/rangerForest.hpp"
#include "containers/nodeDistContainer.hpp"

// // [[Rcpp::export]]
// arma::uvec getPath(arma::umat& nodeIDs, int k) {
//   rangerForest rf;
//   hashMap nodes = rf.nodeIdToHashMap(nodeIDs);
//   return rf.pathToRoot(nodes, k);
// }
// 
// // [[Rcpp::export]]
// int distTest(arma::uvec& path1, arma::uvec& path2) {
//   rangerForest rf;
//   return rf.nodeDistance(path1, path2);
// }
// 
// // [[Rcpp::export]]
// int treeDist(arma::umat& nodeIDs) {
//   rangerForest rf;
//   hashMap nodes = rf.nodeIdToHashMap(nodeIDs);
//   arma::uvec tNodes = rf.terminalNodes(nodeIDs);
//   // calculate path to root
//   
//   return rf.nodeDistance(path1, path2);
// }

// [[Rcpp::export]]
Rcpp::DataFrame terminalNodeDistance(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  return nodeDists.asDataFrame();
}
