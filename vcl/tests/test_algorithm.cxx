/*
  fsm
*/
#include <iostream>
#include <algorithm>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

int test_algorithm_main(int /*argc*/,char* /*argv*/[])
{
  double v[5] = {1.,5.,2.,4.,3.};
  std::sort(v, v+5);

  return 0;
}
