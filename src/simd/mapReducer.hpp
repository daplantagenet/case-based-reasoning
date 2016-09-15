// #ifndef MAPREDUCER_H
// #define MAPREDUCER_H
// 
// // [[Rcpp::depends(RcppNT2)]]
// #include <RcppNT2.h>
// using namespace RcppNT2;
// 
// #include <Rcpp.h>
// using namespace Rcpp;
// 
// 
// class plusReducer {
// public:
//   double init() { 
//     return 0.0; 
//   }
//   
//   template <typename T>
//   T map(const T& lhs, const T& rhs) {
//     return lhs + rhs;
//   }
//   
//   template <typename T>
//   T combine(const T& lhs, const T& rhs) {
//     return lhs + rhs;
//   }
//   
//   template <typename T>
//   auto reduce(const T& t) -> decltype(nt2::sum(t)) {
//     return nt2::sum(t);
//   }
//   
//   template <typename T>
//   void set_parameter() {};
//   
// };
// 
// 
// // map for l1 distance
// class l1DistReducer : public PlusReducer {
// public:
//   template <typename T>
//   T map(const T& lhs, const T& rhs) {
//     return std::abs(lhs - rhs);
//   }
// };
// 
// 
// // map for weighted l1 distance
// class weightedL1DistReducer : public PlusReducer {
// public:
//   template <typename T>
//   T map(const T& lhs, const T& rhs, const T& w) {
//     return w * std::abs(lhs - rhs);
//   }
// };
// 
// 
// // map for lq distance
// class lqDistReducer : public PlusReducer {
// public:
//   template <typename T>
//   T map(const T& lhs, const T& rhs) {
//     return std::pow(lhs - rhs, q_);
//   }
//   
//   void set_parameter(int q = 1) {
//     q_ = q;
//   }
//   
// private:
//   int q_;
// };
// 
// 
// // map for weighted lq distance
// class weightedLqDistReducer : public PlusReducer {
// public:
//   template <typename T>
//   T map(const T& lhs, const T& rhs, const T& w) {
//     return w * std::pow(lhs - rhs, q_);
//   }
//   
//   void set_parameter(int q = 1) {
//     q_ = q;
//   }
//   
// private:
//   int q_;
// };
