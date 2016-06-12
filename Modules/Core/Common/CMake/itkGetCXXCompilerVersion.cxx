#include <iostream>

int main()
{
  #if __cplusplus >= ITK_CXX_TEST_VERSION
    std::cout << __cplusplus << ">=" << ITK_CXX_TEST_VERSION << std::endl;
    return 0;
  #else
    #error "VERSION TEST FAILED" ##ITK_CXX_TEST_VERSION
    std::cout << __cplusplus << ">=" << ITK_CXX_TEST_VERSION << std::endl;
    return 1;
  #endif
}
