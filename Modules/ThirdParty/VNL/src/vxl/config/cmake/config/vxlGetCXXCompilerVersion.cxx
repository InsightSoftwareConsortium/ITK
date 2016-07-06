#include <iostream>

int main()
{
  #if __cplusplus >= VXL_CXX_TEST_VERSION
    std::cout << __cplusplus << ">=" << VXL_CXX_TEST_VERSION << std::endl;
    return 0;
  #else
    #error "VERSION TEST FAILED" VXL_CXX_TEST_VERSION
    std::cout << __cplusplus << ">=" << VXL_CXX_TEST_VERSION << std::endl;
    return 1;
  #endif
}
