// This is vcl/tests/test_multimap.cxx
#include <iostream>
#include <functional>
#include <map>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

typedef std::multimap<int, double, std::less<int> > mymap;

std::ostream &operator<<(std::ostream &s, mymap::value_type const &x)
{
  return s << '(' << x.first << ',' << x.second << ')';
}

int test_multimap_main(int /*argc*/,char* /*argv*/[])
{
  mymap m;
  m.insert(mymap::value_type(0, 2.718281828459045)); // e
  m.insert(mymap::value_type(1, 3.141592653589793)); // pi
  m.insert(mymap::value_type(2, 1.414213562373095)); // sqrt(2)
  m.insert(mymap::value_type(3, 1.61803398874989)); // golden number

  auto b = m.begin();
  auto e = m.end();

  std::cout << "the whole container:" << std::endl;
  for (auto p = b; p != e; ++p)
    std::cout << *p << std::endl;

  std::cout << "lower_bound() and upper_bound():" << std::endl;
  for (int k=-1; k<=4; ++k) {
    std::cout << "k=" << k << std::endl;

    auto lo = m.lower_bound(k);
    std::cout << "  lo: ";
    if (lo==b) std::cout << "begin";
    else if (lo==e) std::cout << "end";
    else std::cout << *lo;
    std::cout << std::endl;

    auto hi = m.upper_bound(k);
    std::cout << "  hi: ";
    if (hi==b) std::cout << "begin";
    else if (hi==e) std::cout << "end";
    else std::cout << *hi;
    std::cout << std::endl;
  }

  return 0;
}
