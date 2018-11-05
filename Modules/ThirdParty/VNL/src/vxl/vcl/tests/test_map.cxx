// This is vcl/tests/test_map.cxx
#include <iostream>
#include <functional>
#include <map>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

int test_map_main(int /*argc*/,char* /*argv*/[])
{
  bool okay = true;
  typedef std::map<int, double, std::less<int> > mymap;
  mymap m;

  m.insert(mymap::value_type(1, 2718));
  m.insert(mymap::value_type(2, 3141));

  for (auto & p : m)
    std::cout << p.first << " " << p.second << std::endl;

  auto i = m.find(3);
  okay = okay && (i == m.end()); // not found (=OK)
  i = m.find(2);
  okay = okay && (i != m.end()) && ((*i).second == 3141);

  if ( okay )
    return 0;
  else
    return 1;
}
