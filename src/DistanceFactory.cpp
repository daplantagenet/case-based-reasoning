#include "DistanceFactory.h"
#include "Utility.h"
#include "DistanceRF.h"


std::shared_ptr<Distance> DistanceFactory::createDistanceFunction(Rcpp::List& attrs, Rcpp::List& arguments) {
  using namespace utility;
  std::string distName = attrs["method"];
  std::shared_ptr<Distance> distanceFunction = NULL;
  
  Rcpp::Rcout << "Proximity" << std::endl;
  if (isEqualStr(distName, "Proximity")) {
    Rcpp::Rcout << "Proximity" << std::endl;
    int nTrees = 0;
    if (arguments.containsElementNamed("nTrees")) {
      nTrees = Rcpp::as<int>(arguments["nTrees"]);
    } else {
      Rcpp::stop("Parameter nTrees is neccessary for Proximity Distance.");
    }
    DistanceRFProximity dist(nTrees);
    return std::make_shared<DistanceRFProximity>(dist);
  } else if (isEqualStr(distName, "Depth")) {
    return distanceFunction;
    // distanceFunction = std::make_shared<DistanceRFDepth>();
  }
  
  return distanceFunction;
}
