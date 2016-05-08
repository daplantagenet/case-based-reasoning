#include <Rcpp.h>
using namespace Rcpp;
#include <map>
#include <vector>
#include <utility>
#include <algorithm>
using namespace std;

typedef pair<pair <int, int>, double>  TElementOne;
typedef std::vector < TElementOne > TVectorOne;

TVectorOne getTryForOne(DataFrame df) {
    Rcpp::DataFrame copy(df);
    Rcpp::NumericMatrix orig=internal::convert_using_rfunction(copy, "as.matrix");
    
    Rcpp::NumericVector myv, mynv, nodeID, g(2), gT,empty;
    Rcpp::NumericVector x,y,dist,li,lj;
    
    Rcpp::NumericVector vec=orig(3,_); 
    //Rcpp::Rcout<<vec;
    
    int i=0,k;
    NumericVector::iterator it;
    
    // create myv and mynv 
    for(it = vec.begin() ; it != vec.end(); ++it,++i) 
        if ( internal::Rcpp_IsNA((*it)) || 
             internal::Rcpp_IsNaN(*it)) 
        {
            mynv.push_back(i);
            myv.push_back(1);
        }
        else myv.push_back(0);
        
        int size=mynv.size();
        
        //  nodeID <- x$nodeID[mynv]
        vec=orig(1,_);
        nodeID=empty;
        
        for( i=0; i<size; i++)
        {
            k=vec[mynv[i]];
            nodeID.push_back(k);
        }
        
        
        // myil <- list()
        // for (i in 2:length(myv)) 
        List myil;
        g[0]=g[1]=0;
        int j;
        
        // for( i=1; i< myv.size() ; i++) { 
        for( i=1; i< myv.size() ; i++) { 
            
            gT=empty;
            
            if (myv[i - 1]) {
                
                // myil <- append(myil, list(g)) 
                myil.push_back(g); 
                // g <- c(g[length(g)], g[2:(length(g) - 1)])
                gT.push_back(g[g.size()-1]);        
                j=1;    
                while(j<g.size()-1)gT.push_back(g[j++]); 
                g=gT;
                
            } else {
                
                gT.push_back(g[0]+1);
                j=1;    
                while(j<g.size())gT.push_back(g[j++]);
                gT.push_back(g[0]+1);
                g=gT;     
            }
            
        }
        
        myil.push_back(g); 
        
        
        //   length: (length(mynv)^2 - length(mynv)) / 2; 
        //    int l=(size*size-size) /2;   
        int el;
        TVectorOne rows;
        for( i=0; i<size-1; i++) {
            for( j=i+1; j<size; j++){ 
                
                //id <- which.min(myil[[i]][2:length(myil[[i]])] %in% myil[[j]][2:length(myil[[j]])])
                li=myil[i];
                lj=myil[j];      
                int id=5, flag;
                for( int i1=1; i1<li.size(); i1++) {
                    el=li[i1];
                    flag=0;
                    id=1;
                    
                    for( int i2=1; i2<lj.size(); i2++)
                        if (el == lj[i2]) {flag=1;break;}
                        
                        if (!flag) {id = i1; break;}
                }
                
                
                //el=nodeID[i];
                //x.push_back(el);
                
                //el=nodeID[j];
                //y.push_back(el);
                
                //myil[[i]][1] + myil[[j]][1] - 2 * myil[[i]][2:length(myil[[i]])][id] + 2
                
                el=li[0] + lj[0] - 2 * li[id] + 2;
                //dist.push_back(el);
                
                TElementOne row;
                
                row.first.first=nodeID[i];
                row.first.second=nodeID[j];
                row.second=el;
                rows.push_back(row);
                  
            }
        }
        
        /*
        return Rcpp::List::create(
            Rcpp::Named("myv") = myv,
        Rcpp::Named("mynv") = mynv,
        Rcpp::Named("nodeID") = nodeID,
        Rcpp::Named("l") = l,
        Rcpp::Named("g") = g,
        Rcpp::Named("x") = x,
        Rcpp::Named("y") = y,
        Rcpp::Named("dist") = dist,
        Rcpp::Named("myil") = myil);
        
        */
        
        return rows;
}

typedef pair<pair <int, int>, vector <double> > TElement;
typedef std::map <pair <int, int>,  vector <double> >  TMap;



DataFrame getDataFrame(TMap all, int nTree) {
    
    Rcpp::NumericVector x, y, dist, null;
    
    Rcpp::CharacterVector namevec;
    List lists(nTree+2);
    
    std::vector<double> v(nTree,-1);
    std::vector<vector <double> > dists(nTree); 
    
    
    namevec.push_back("x");
    namevec.push_back("y");
    
    std::string namestem = "tree_";
    for(TMap::const_iterator it1 = all.begin(); it1 != all.end(); ++it1){
        x.push_back(it1->first.first);
        y.push_back(it1->first.second);
        
        for(int n=0;n<nTree;n++){
            dists[n].push_back(it1->second[n]);
        }
        
    }
    
    lists[0] = x; 
    lists[1] = y; 
    
    for(int n=0;n<nTree;n++){
        namevec.push_back(namestem + std::to_string(n+1));
        lists[n+2]=dists[n];
    }
    
    lists.attr("names") = namevec;
    Rcpp::DataFrame dfout(lists);
    return dfout;
}

   
TMap getMap(DataFrame df, int nTree) {
    TMap all;
    TVectorOne one;
    Rcpp::DataFrame obj(df);
   
    Rcpp::NumericVector treeID, x, y, dist,null;
    List lists(nTree+2);
   
    Rcpp::DataFrame empty;
    Rcpp::NumericMatrix mat=internal::convert_using_rfunction(df, "as.matrix");
    Rcpp::DataFrame treeAgeID;  
 
  NumericVector::iterator  it2; 
  
  std::vector<double> v(nTree,-1);
  std::vector<vector <double> > dists(nTree); 
  
    int i=0;
    treeID = obj["treeID"];
   for (int n = 1; n<=nTree;  n++){
       
        for (i=0,it2 = treeID.begin(); it2 != treeID.end(); i++, ++it2){
            if(*it2 == n){
                treeAgeID.push_back(mat(i,_)); 
            }
        }
         
    one=getTryForOne(treeAgeID);
    treeAgeID=empty;    
        
        
        for( i=0; i<one.size();i++){
            TElement  node;
            node.first=one[i].first;
            
            if ( all.find(node.first) == all.end() ){
                    
                 node.second=v;
                 all.insert(node); 
                 all.at(node.first)[n-1]=one[i].second;
            }
            else{
                all.at(node.first)[n-1]=one[i].second;
            }
                
         }
    }
   return all;
}

// [[Rcpp::export]]
// 
DataFrame getDistForTreesCPP(DataFrame df, int nTree) {
    TMap all=getMap(df,nTree);
    return getDataFrame(all,nTree);
}
    
// [[Rcpp::export]]
// 
double getGijt(DataFrame df, int nTree, int currentX, int currentY, int index) {
    TMap all=getMap(df,nTree);
    Rcpp::NumericVector treeID, x, y, dist,null;
    TElement  node;
    if(index>nTree+2)return -1;
    if(index<3)return -1;
    
    node.first.first=currentX;
    node.first.second=currentY;
    if ( all.find(node.first) == all.end() ) return -1;
    double d=all.at(node.first)[index-3];
    return d;
}



// bool positive(int x) { return x>0; }

// [[Rcpp::export]]
// 
double getDXiXj(DataFrame df, int nTree, int currentX, int currentY, int w=2) {
    TMap all=getMap(df,nTree);
    
    TElement  node;
    node.first.first=currentX;
    node.first.second=currentY;
    if ( all.find(node.first) == all.end() ) return 1;
    std::vector <double> v=all.at(node.first);
    std::vector <double>::iterator it;
    
    //int numEvens = count_if(v.begin(), v.end(), positive);
    
    double sum = 0, d;
    int count=0; //nTree;
    for(it=v.begin(); it!=v.end();++it){
        d=*it;
       // Rcpp::Rcout <<"it=" << d<< "\n";
        if(d>0){
          sum+=1. / exp(w * d);
          count++;
      }  
    }
        
    
   
    
    Rcpp::Rcout << "sum = " << sum<< "\n";
    Rcpp::Rcout << "w = " << w<< "\n";
    return 1-sum/count;
}





/*** R
# The testing

#treeAgeT <- read.csv("~/Documents/upwork/Rcpp/AllTrees_my/treeAge.csv")
#treeAgeT <-treeAgeT [-1]
#nTree<-3

#DistTreeAll<-getDistForTreesCPP(treeAgeT, nTree)
#getGijt(treeAgeT, nTree, 2, 23, 4)
#getDXiXj(treeAgeT, nTree, 2, 23,0)
#getDXiXj(treeAgeT, nTree, 1, 29)
#getDXiXj(treeAgeT, nTree, 1, 29,0)
#getDXiXj(treeAgeT, nTree, 2, 29, 0)
*/