#include "Utility.h"

namespace utility {
  bool isEqualStr(std::string& str1, std::string str2) {
    return str1.compare(str2) == 0;
  }
}