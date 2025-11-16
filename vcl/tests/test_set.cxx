#include <iostream>
#include <functional>
#include <set>
#ifdef _MSC_VER
#  include "vcl_msvc_warnings.h"
#endif

int
test_set_main(int /*argc*/, char * /*argv*/[])
{
  using myset = std::set<int, std::less<int>>;
  myset s;

  s.insert(1);

  for (const myset::value_type p : s)
    std::cout << p << std::endl;
  return 0;
}
