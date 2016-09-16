#include "distanceAPI.hpp"

RCPP_MODULE(distance) {
  class_< distanceAPI >( "distanceAPI" )
  //<vocab_size, word_vec_size, x_max, learning_rate, grain_size, max_cost, alpha>
  .constructor<size_t, size_t, uint32_t, double, uint32_t, double, double>()
  .method( "get_word_vectors", &GloveFitter::get_word_vectors, "returns word vectors")
  .method( "set_cost_zero", &GloveFitter::set_cost_zero, "sets cost to zero")
  .method( "fit_chunk", &GloveFitter::fit_chunk, "process TCM data chunk")
  ;
}
