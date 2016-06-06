// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

#include <map>
#include <vector>
#include <algorithm>

using namespace std;

typedef pair<pair <int, int>, double>  TElementOne;
typedef std::vector < TElementOne > TVectorOne;
typedef pair<pair <int, int>, vector<double> > TElement;
typedef unordered_map< pair<uint32_t, uint32_t>, vector <double> >  TuMap;

namespace std {
template <>
struct hash<std::pair<uint32_t, uint32_t>>
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


DataFrame umap_to_dataframe(TuMap treeMap, int nTree) {
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

// case 1, case 2, index of the tree (from 0)
double get_node_distance(const TuMap &treeMap, int currentX, int currentY, int t) {
  if (treeMap.find(make_pair(currentX, currentY)) == treeMap.end()) return -1.;
  double d = treeMap.at(make_pair(currentX, currentY))[t];
  return d;
}


#if RCPP_PARALLEL_USE_TBB

struct rf_distance : public Worker {
  const RMatrix<double> mMember;
  const std::size_t nTree;
  const std::size_t w;
  const TuMap treeMap;
  
  RMatrix<double> mDist;
  rf_distance(const NumericMatrix& mMember, 
             const std::size_t nTree, 
             const TuMap& treeMap, 
             NumericMatrix mDist, 
             const std::size_t w=2)
    : mMember(mMember), nTree(nTree), w(w), treeMap(treeMap), mDist(mDist) {}
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      for (std::size_t j = 0; j < i; j++) {      
        RMatrix<double>::Row row1 = mMember.row(i);
        RMatrix<double>::Row row2 = mMember.row(j);
        double sum=0.0, d;
        int tmpTree = 1;
        for (int t=0; t < nTree; t++) { 
          if (row1[t] < row2[t]) {
            d = get_node_distance(treeMap, row1[t], row2[t], t);
          } else {
            d = get_node_distance(treeMap, row2[t], row1[t], t);
          }
          if (d > 0) {
            sum += 1. / std::pow(d, w);
            tmpTree += 1;
          }
        }
        mDist(i, j) = mDist(j, i) = 1. - sum * 1. / nTree;
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rf_distance_matrix(DataFrame df,  DataFrame member, int w=2){
  Rcpp::DataFrame memb(member);
  Rcpp::NumericMatrix matMember=internal::convert_using_rfunction(memb, "as.matrix");
  std::size_t nTree = memb.length(), wPar=w;
  TuMap treeMap = create_hash_map(df, nTree);
  std::size_t size = memb.nrows();
  NumericMatrix mDist(size, size);
  rf_distance rf_distance(matMember, nTree, treeMap,  mDist, wPar);
  parallelFor(0, matMember.nrow(), rf_distance);
  return mDist;
}

#else

NumericMatrix rf_distance_matrix(DataFrame df, DataFrame member, int w=2) {
  DataFrame memb(member); 
  int nTree = memb.length();
  TuMap treeMap = create_hash_map(df, nTree);
  Rcpp::NumericMatrix mat = internal::convert_using_rfunction(memb, "as.matrix");
  int size = memb.nrows();
  NumericMatrix mDist(size, size);
  for (int i=0; i < size - 1; i++) {
    for (int j=i + 1; j < size; j++) { 
      NumericMatrix::Row row1 = mat.row(i);
      NumericMatrix::Row row2 = mat.row(j);
      double sum=0.0, d;
      for (int t=0; t < nTree; t++) { 
        if (row1[t] < row2[t]) {
          d = get_node_distance(treeMap, row1[t], row2[t], t);
        } else {
          d = get_node_distance(treeMap, row2[t], row1[t], t);
        }
        if (d > 0) {
          sum += 1. / std::pow(.1 * d, w);
        }
      }
      mDist(i, j) = mDist(j, i) = 1. - sum * 1. / nTree;
    }
  }
  return mDist;
}

#endif

// [[Rcpp::export]]
NumericMatrix get_rf_distance_matrix(DataFrame df, DataFrame member, int w=2) {
  return rf_distance_matrix(df, member, w);
}

// [[Rcpp::export]]
DataFrame get_node_distances(DataFrame df, int nTree) {
  TuMap treeMap = create_hash_map(df, nTree);
  return umap_to_dataframe(treeMap, nTree);
}

