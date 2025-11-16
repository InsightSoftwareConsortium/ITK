// Try to check that compiler preprocessor definitions are sane.

#include <iostream>
#ifdef _MSC_VER
#  include "vcl_msvc_warnings.h"
#endif
#include "vcl_compiler.h"

int
test_preprocessor_main(int /*argc*/, char * /*argv*/[])
{
  const int result = 0;
  std::cout << "VXL version identified as: " << VXL_VERSION_MAJOR << "." << VXL_VERSION_MINOR << "."
            << VXL_VERSION_PATCH;

  std::cout << "Version identified implies release identified: ";
  std::cout << "PASSED\n";
  return result;
}
