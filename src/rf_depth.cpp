// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>

#include <algorithm>
#include <vector>
#include "sort.h"

using namespace Rcpp;
using namespace RcppParallel;
using namespace std;

typedef pair<pair <int, int>, double>  TElementOne;
typedef std::vector < TElementOne > TVectorOne;
typedef pstd::air<std::pair <int, int>, std::vector<double> > TElement;
typedef std::unordered_map< std::pair<std::uint32_t, std::uint32_t>, std::vector <double> >  TuMap;


// custom hash function for pair
namespace std {
template <>
struct std::hash<std::pair<uint32_t, uint32_t> >
{
  inline uint64_t operator()(const std::pair<uint32_t, uint32_t>& k) const
  {
    //should produce no collisions
    //http://stackoverflow.com/a/24693169/1069256
    //return f << (CHAR_BIT * sizeof(size_t) / 2) | s;
    //http://stackoverflow.com/questions/2768890/how-to-combine-two-32-bit-integers-into-one-64-bit-integer?lq=1
    return (uint64_t) k.first << 32 | k.second;
  }
};
}


TVectorOne getTryForOne(DataFrame df) {
  Rcpp::DataFrame copy(df);
  Rcpp::NumericMatrix orig=internal::convert_using_rfunction(copy, "as.matrix");
  
  Rcpp::NumericVector myv, mynv, nodeID, g(2), gT, empty;
  Rcpp::NumericVector x, y, dist, li, lj;
  
  Rcpp::NumericVector vec=orig(3, _); 
  
  int i=0, k;
  NumericVector::iterator it;
  
  for(it = vec.begin(); it != vec.end(); ++it, ++i) 
    if (internal::Rcpp_IsNA((*it)) || internal::Rcpp_IsNaN(*it)) {
      mynv.push_back(i);
      myv.push_back(1);
    } else {
      myv.push_back(0);
    }
    int size=mynv.size();
    
    vec = orig(1, _);
    nodeID = empty;
    for (i=0; i < size; i++) {
      k = vec[mynv[i]];
      nodeID.push_back(k);
    }
    
    List myil;
    g[0] = g[1] = 0;
    int j;
    for( i=1; i< myv.size(); i++) { 
      gT=empty;
      if (myv[i - 1]) {
        myil.push_back(g); 
        gT.push_back(g[g.size()-1]);        
        j=1;    
        while(j < g.size() - 1)gT.push_back(g[j++]); 
        g=gT;
        
      } else {
        gT.push_back(g[0] + 1);
        j=1;    
        while(j < g.size())gT.push_back(g[j++]);
        gT.push_back(g[0]+1);
        g=gT;     
      }
    }
    
    myil.push_back(g); 
    int el;
    TVectorOne rows;
    for(i = 0; i < size - 1; i++) {
      for(j = i + 1; j < size; j++) { 
        li = myil[i];
        lj = myil[j];      
        int id=5, flag;
        for(int i1=1; i1 < li.size(); i1++) {
          el=li[i1];
          flag=0;
          id=1;
          for (int i2=1; i2<lj.size(); i2++)
            if (el == lj[i2]) {
              flag=1;
              break;
            }
            if (!flag) {
              id = i1; break;
            }
        }
        el = li[0] + lj[0] - 2 * li[id] + 2;
        TElementOne row;
        row.first.first=nodeID[i];
        row.first.second=nodeID[j];
        row.second=el;
        rows.push_back(row);
      }
    }
    return rows;
}


DataFrame tumap_to_dataframe(TuMap treeMap, int nTree) {
  Rcpp::NumericVector x, y, dist, null;
  Rcpp::CharacterVector namevec;
  List lists(nTree + 2);
  
  std::vector<double> v(nTree, -1);
  std::vector<vector <double> > dists(nTree); 
  namevec.push_back("x");
  namevec.push_back("y");
  
  std::string namestem = "tree_";
  for(TuMap::const_iterator it1 = treeMap.begin(); it1 != treeMap.end(); ++it1) {
    x.push_back(it1->first.first);
    y.push_back(it1->first.second);
    for(int n=0; n < nTree; n++) {
      dists[n].push_back(it1->second[n]);
    }
  }
  lists[0] = x; 
  lists[1] = y; 
  for (int n=0; n < nTree; n++) {
    namevec.push_back(namestem + std::to_string(n + 1));
    lists[n + 2] = dists[n];
  }
  lists.attr("names") = namevec;
  Rcpp::DataFrame dfout(lists);
  return dfout;
}


TuMap create_hash_map(DataFrame df, int nTree) {
  TuMap treeMap;
  TVectorOne one;
  Rcpp::DataFrame obj(df);
  
  Rcpp::NumericVector treeID, x, y, dist,null;
  List lists(nTree + 2);
  
  Rcpp::DataFrame empty;
  Rcpp::NumericMatrix mat = internal::convert_using_rfunction(df, "as.matrix");
  Rcpp::DataFrame treeAgeID;  
  
  NumericVector::iterator  it2; 
  
  std::vector<double> v(nTree, -1);
  std::vector<vector <double> > dists(nTree); 
  
  int i=0;
  treeID = obj["treeID"];
  for (int n = 1; n<=nTree;  n++) {
    for (i=0, it2 = treeID.begin(); it2 != treeID.end(); i++, ++it2){
      if(*it2 == n) {
        treeAgeID.push_back(mat(i, _)); 
      }
    }
    one = getTryForOne(treeAgeID);
    treeAgeID = empty;    
    for(i=0; i < one.size(); i++) {
      TElement node;
      node.first=one[i].first;
      if (treeMap.find(make_pair(one[i].first.first, one[i].first.second)) == treeMap.end()) {
        treeMap[make_pair(one[i].first.first, one[i].first.second)] = v;
        treeMap.at(make_pair(one[i].first.first, one[i].first.second))[n - 1] = one[i].second;
      } else {
        treeMap.at(make_pair(one[i].first.first, one[i].first.second))[n - 1] = one[i].second;
      }
    }
  }
  return treeMap;
}


double get_node_distance(const TuMap &treeMap, int currentX, int currentY, int t) {
  if (treeMap.find(make_pair(currentX, currentY)) == treeMap.end()) return -1.;
  double d = treeMap.at(make_pair(currentX, currentY))[t];
  return d;
}


#if RCPP_PARALLEL_USE_TBB

struct rf_distance : public Worker {
  const RMatrix<double> member;
  const RMatrix<double> memberQuery;
  const std::size_t nTree;
  const std::size_t w;
  const TuMap treeMap;
  
  RMatrix<double> mDist;
  rf_distance(
    const NumericMatrix member, 
    const NumericMatrix memberQuery, 
    const std::size_t nTree, 
    const TuMap treeMap, 
    NumericMatrix mDist, 
    const std::size_t w=2)
    : member(member), memberQuery(memberQuery), nTree(nTree), w(w), treeMap(treeMap), mDist(mDist) {}
  void operator()(std::size_t begin, std::size_t end) {
    int sizeQuery = memberQuery.nrow();
    for (std::size_t i=begin; i<end; i++) {
      RMatrix<double>::Row row1 = member.row(i);
      for (std::size_t j=0; j<sizeQuery; j++) {      
        RMatrix<double>::Row row2 = memberQuery.row(j);
        double sum=0.0, d;
        int tmpTree = 1;
        for (int t=0; t < nTree; t++) { 
          if (row1[t] < row2[t]) {
            d = get_node_distance(treeMap, row1[t], row2[t], t);
          } else if (row1[t] > row2[t]) {
            d = get_node_distance(treeMap, row2[t], row1[t], t);
          } else {
            d = 0.0;
            sum += 0.0;
            tmpTree += 1;
          }
          if (d > 0) {
            sum += d; //1. / pow(d, w);
            tmpTree += 1;
          }
        }
        mDist(i, j) = log(sum + 1.); // 1. - sum * 1. / nTree;
      }
    }
  }
};

NumericMatrix rf_distance_matrix(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w=2){
  Rcpp::Rcout << "Start Parallel Calculation!" << std::endl;
  std::size_t nTree = member.ncol(), wPar=w;
  std::size_t size = member.nrow();
  std::size_t sizeQuery = memberQuery.nrow();
  TuMap treeMap = create_hash_map(df, nTree);
  NumericMatrix mDist(size, sizeQuery);
  rf_distance rf_distance(member, memberQuery, nTree, treeMap,  mDist, wPar);
  parallelFor(0, member.nrow(), rf_distance);
  return mDist;
}


struct rf_knn : public Worker {
  const RMatrix<double> member;
  const RMatrix<double> memberQuery;
  const int nTree;
  const int w;
  const int k;
  const TuMap treeMap;
  
  RMatrix<double> mDist;
  RMatrix<double> mOrder;
  
  rf_knn(
    const NumericMatrix member, 
    const NumericMatrix memberQuery, 
    const std::size_t nTree, 
    const std::size_t w,
    const std::size_t k, 
    const TuMap treeMap, 
    NumericMatrix mDist,
    NumericMatrix mOrder
  ) : member(member), memberQuery(memberQuery), nTree(nTree), w(w), k(k), 
  treeMap(treeMap), mDist(mDist), mOrder(mOrder) {}
  void operator()(std::size_t begin, std::size_t end) {
    int size = member.nrow();
    double sum = 0.0;
    int tmpTree;
    double d;
    vector<double> tmpDist(size, 0);
    map<int, double> tmp;
    for (size_t i=begin; i<end; ++i) {
      RMatrix<double>::Row row1 = member.row(i);
      for (size_t j=0; j<size; ++j) { 
        RMatrix<double>::Row row2 = memberQuery.row(j);
        tmpTree = 1;
        sum = 0.0;
        for (int t=0; t < nTree; t++) { 
          if (row1[t] < row2[t]) {
            d = get_node_distance(treeMap, row1[t], row2[t], t);
          } else if (row1[t] > row2[t]) {
            d = get_node_distance(treeMap, row2[t], row1[t], t);
          } else {
            d = 0.0;
            sum += 0.0;
            tmpTree += 1;
          }
          if (d > 0) {
            sum += d;
            tmpTree += 1;
          }
        }
        tmp[j] = sum;
      }
      std::multimap<double, int> dst = flip_map(tmp);
      multimap<double, int>::const_iterator it = dst.cbegin();
      for (size_t l=0; l<k; ++l) {
        mDist(i, l) = log(it->first + 1.);
        mOrder(i, l) = it->second + 1;
        it++;
      }
    }
  }
};

List rf_knn_matrix(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w=2, int k = 1) {
  Rcpp::Rcout << "Start Parallel Calculation!" << std::endl;
  int nTree = member.ncol();
  int sizeQuery = memberQuery.nrow();
  
  TuMap treeMap = create_hash_map(df, nTree);
  NumericMatrix mDist(sizeQuery, k);
  NumericMatrix mOrder(sizeQuery, k);
  
  rf_knn rf_knn(member, memberQuery, nTree, w, k, treeMap, mDist, mOrder);
  parallelFor(0, sizeQuery, rf_knn);
  return Rcpp::List::create(
    Rcpp::Named("distance") = mDist,
    Rcpp::Named("order")    = mOrder
  );
}

#else

// rf distance matrix
NumericMatrix rf_distance_matrix(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w=2) {
  Rcpp::Rcout << "Start Single Calculation!" << std::endl;
  int nTree = member.ncol();
  int size = member.nrow();
  int sizeQuery = memberQuery.nrow();
  TuMap treeMap = create_hash_map(df, nTree);
  NumericMatrix mDist(size, sizeQuery);
  for (int i=0; i < size; i++) {
    NumericMatrix::Row row1 = member.row(i);
    for (int j=0; j < sizeQuery; j++) { 
      NumericMatrix::Row row2 = memberQuery.row(j);
      double sum=0.0, d;
      int tmpTree = 1;
      for (int t=0; t < nTree; t++) { 
        if (row1[t] < row2[t]) {
          d = get_node_distance(treeMap, row1[t], row2[t], t);
        } else if (row1[t] > row2[t]) {
          d = get_node_distance(treeMap, row2[t], row1[t], t);
        } else {
          d = 0;
          sum += 1.;
          tmpTree += 1;
        }
        if (d > 0) {
          sum += 1. / pow(d, w);
          tmpTree += 1;
        }
      }
      mDist(i, j) = 1. - sum * 1. / nTree;
    }
  }
  return mDist;
}

// rf knn
List rf_knn_matrix(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w=2, int k = 1) {
  Rcpp::Rcout << "Start Single Calculation!" << std::endl;
  int nTree = member.n_cols;
  int size = member.n_rows;
  int sizeQuery = memberQuery.n_rows;
  
  TuMap treeMap = create_hash_map(df, nTree);
  
  NumericMatrix mDist(sizeQuery, k);
  NumericMatrix mOrder(sizeQuery, k);
  
  arma::colvec tmpDist(size);
  arma::uvec order(size);
  tmpDist = arma::zeros<arma::vec>(size);
  int x, y;
  for (int i=0; i<sizeQuery - 1; ++i) {
    for (int j=0; j<size; ++j) { 
      double d;
      int tmpTree = 1;
      for (int t=0; t<nTree; t++) { 
        x = member.row(j)(t);
        y = memberQuery.row(i)(t);
        if (x < y) {
          d = get_node_distance(treeMap, x, y, t);
        } else if (x > y) {
          d = get_node_distance(treeMap, x, y, t);
        } else {
          d = 0;
          tmpDist(j) += 1.;
          tmpTree += 1;
        }
        if (d > 0) {
          tmpDist(j) += 1. / pow(d, w);
          tmpTree += 1;
        }
      }
    }
    Rcpp::Rcout << "Start Single Calculation!" << k << std::endl;
    tmpDist = 1. - tmpDist * 1. / nTree;
    order = arma::sort_index(tmpDist, 0);
    for (size_t l=0; l<k; ++l) {
      mDist(i, l) = tmpDist(order(l));
      mOrder(i, l) = order(l) + 1;
    }
    tmpDist = arma::zeros<arma::vec>(size);
  }
  return Rcpp::List::create(
    Rcpp::Named("distance") = mDist,
    Rcpp::Named("order")    = mOrder
  );
}
#endif

// [[Rcpp::export]]
NumericMatrix get_rf_distance_matrix(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w=2) {
  return rf_distance_matrix(df, member, memberQuery, w);
}

// [[Rcpp::export]]
List rf_knn(DataFrame df, NumericMatrix member, NumericMatrix memberQuery, int w = 2, int k = 5) {
  return rf_knn_matrix(df, member, memberQuery, w, k);
}


// [[Rcpp::export]]
DataFrame get_node_distances(DataFrame df, int nTree) {
  TuMap treeMap = create_hash_map(df, nTree);
  return tumap_to_dataframe(treeMap, nTree);
}

