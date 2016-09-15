#ifndef RANGERFOREST_H
#define RANGERFOREST_H

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <unordered_map>

#include "../containers/nodeDistContainer.hpp"

typedef std::unordered_map<int, int> hashMap;
typedef std::unordered_map<int, hashMap> treeHashMap;
typedef std::unordered_map<int, arma::uvec> hashVec;
typedef std::unordered_map<int, hashVec> treeHashVec;


//
class rangerForest {
public: 
  // nodeIDs: 
  //    column 1: tree id
  //    column 2: node id
  //    column 3: childNodeIDs 1
  //    column 4: childNodeIDs 1
  rangerForest(arma::umat& nodeIDs) {
    nodeIDs_ = nodeIDs;
    this->treeIndex();
    this->getPaths();
  };
  
  // calculate terminal node distance for each tree
  RfDistContainer nodeDistance() {
    auto nTrees = treeIndex_.size() - 1;
    RfDistContainer rfDist;
    rfDist.setNTree(nTrees);
    int d = 0;
    for (auto t=0;t<nTrees;++t) {
      hashVec hv = hp_[t+1];
      for (auto it1 = hv.cbegin();it1!=hv.cend();++it1) {
        auto itTemp = it1;
        ++itTemp;
        for (auto it2=itTemp;it2!=hv.cend();++it2) {
          d = this->terminalNodeDistance(it1->second, it2->second);
          rfDist.addValue(it1->first, it2->first, t, d);  
        } 
      }
    }
    return rfDist;
  };
  
private:
  // get the tree indices
  void treeIndex() {
    auto nTrees = nodeIDs_.col(0)(nodeIDs_.n_rows - 1); 
    // tree index starts from 1, so this vector has length n trees + 1
    arma::uvec treeIndex(nTrees + 1);
    treeIndex.fill(0);
    auto tmpTree = 1;
    int nrow = nodeIDs_.n_rows;
    for (auto i=0;i<nrow;++i) {
      if (tmpTree != nodeIDs_.col(0)(i)) {
        treeIndex(tmpTree) = i;
        ++tmpTree;
      }
    }
    treeIndex(nTrees) = nrow;
    treeIndex_ = treeIndex;
  };
  
  // get indices of terminal nodes for all trees
  hashVec terminalNodes() {
    hashVec treeTerminalNodes;
    for (auto t=0;t<treeIndex_.size()-1;++t) {
      Rcpp::NumericVector ind;
      for (auto i=treeIndex_(t);i<treeIndex_(t+1);++i) {
        if (nodeIDs_.col(2)(i) == 0) {
          ind.push_back(i - treeIndex_(t) + 1);
        }
      }
      treeTerminalNodes[t + 1] = Rcpp::as<arma::uvec>(Rcpp::wrap(ind));
    }
    return treeTerminalNodes;
  };
  
  // transform matrix to hashmap for all trees
  treeHashMap nodeIdToHashMap() {
    treeHashMap treeNodes;
    for (auto t=0;t<treeIndex_.size()-1;++t) {
      hashMap nodes;
      for (auto i=treeIndex_(t);i<treeIndex_(t+1);++i) {
        if (nodeIDs_(i, 2) != 0) {
          nodes[nodeIDs_(i, 2)] = nodeIDs_(i, 1);
        }
        if (nodeIDs_(i, 3) != 0) {
          nodes[nodeIDs_(i, 3)] = nodeIDs_(i, 1);
        }
      }
      treeNodes[t + 1] = nodes;
    }
    return treeNodes;
  };
  
  // get paths to root for all terminal nodes for all tree
  void getPaths() {
    // transform nodeID matrix to hashmap
    treeHashMap nodes = this->nodeIdToHashMap();
    // get terminal nodes
    hashVec tNodes = this->terminalNodes();
    auto nTrees = treeIndex_.size() - 1;
    for (auto t=0;t<nTrees;++t) {
      hashVec hv;
      // get for each terminal node the path to root
      for (auto tn : tNodes[t + 1]) {
        hv[tn] = this->pathToRoot(nodes[t + 1], tn);
      }
      hp_[t + 1] = hv;
    }
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
  int terminalNodeDistance(arma::uvec path1, arma::uvec path2) {
    int n = path1.size();
    int m = path2.size();
    for (auto i=0;i<n;++i) {
      for (auto j=0;j<m;++j) {
        if (path1(i) == path2(j)) {
          return i + j;
        }
      }
    }
    // should not happen; at least root node is common node
    return -99;
  };
  
  arma::uvec treeIndex_;
  arma::umat nodeIDs_;
  treeHashVec hp_;
};

#endif
