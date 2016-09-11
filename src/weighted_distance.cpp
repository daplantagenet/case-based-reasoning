// // [[Rcpp::depends(RcppParallel)]]
// #include <RcppParallel.h>
// #include <Rcpp.h>
// 
// using namespace RcppParallel;
// using namespace Rcpp;
// 
// #if RCPP_PARALLEL_USE_TBB
// 
// struct weighted_dist : public Worker {
//   const NumericMatrix x;
//   const NumericMatrix query;
//   const NumericVector weights;
//   const int nVars;
//   NumericMatrix& output;
//   weighted_dist(const NumericMatrix x,
//                 const NumericMatrix query,
//                 const NumericVector weights,
//                 const int nVars,
//                 NumericMatrix& output)
//     : x(x), query(query), weights(weights), nVars(nVars), output(output) {}
//   
//   void operator() (std::size_t begin, std::size_t end) {
//     for (std::size_t j = begin; j < end; ++j) {
//       for (std::size_t l = 0; l < nVars; ++l) {
//         output(Rcpp::_, j) = output(Rcpp::_, j) + abs(weights[l] * (x(Rcpp::_, l) - query(j, l)));
//       }
//     }
//   }
// };
// 
// NumericMatrix get_weighted_distance(NumericMatrix x, NumericMatrix query, NumericVector weights) {
//   const int nVars = query.ncol();
//   const int nRowQuery = query.nrow();
//   const int nRow = x.nrow();
//   NumericMatrix output(nRow, nRowQuery);
//   weighted_dist weighted_dist(x, query, weights, nVars, output);
//   parallelFor(0, nRowQuery, weighted_dist);
//   return output;
// }
// 
// #else
// 
// NumericMatrix get_weighted_distance(NumericMatrix x, NumericMatrix query, NumericVector weigths) {
//   const int nVars = query.ncol();
//   const int nRow = x.nrow();
//   const int nRowQuery = query.nrow();
//   NumericMatrix output(nRow, nRowQuery);
//   for (std::size_t i = 0; i < nrow; ++i) {
//     for (std::size_t j = 0; j < nRowQuery; ++j) {
//       for (std::size_t l = 0; l < nVars; ++l) {
//         output(i, j) += std::abs(weigths[l] * (x(i, l) - query(j, l)));
//       }
//     }
//   }
//   return output;
// }
// 
// #endif
// 
// // [[Rcpp::export]]
// NumericMatrix weighted_distance(NumericMatrix x, NumericMatrix query, NumericVector weigths) {
//   return get_weighted_distance(x, query, weigths);
// }
