#include "DistanceFactory.h"
#include "Utility.h"
#include "DistanceRF.h"



std::shared_ptr<Distance> DistanceFactory::createDistanceFunction(Rcpp::List& attrs, Rcpp::List& arguments) {
  using namespace utility;
  std::string distName = attrs["method"];
  std::shared_ptr<Distance> distanceFunction = NULL;
  
  if (isEqualStr(distName, "Proximity")) {
    int nTrees = 0;
    if (arguments.containsElementNamed("nTrees")) {
      nTrees = Rcpp::as<int>(arguments["nTrees"]);
    } else {
      Rcpp::stop("Parameter nTrees is neccessary for Proximity Distance.");
    }
    distanceFunction = std::make_shared<DistanceRFProximity>(nTrees);
  } else if (isEqualStr(distName, "Depth")) {

    // distanceFunction = std::make_shared<DistanceRFDepth>();
  }
  return distanceFunction;
}
