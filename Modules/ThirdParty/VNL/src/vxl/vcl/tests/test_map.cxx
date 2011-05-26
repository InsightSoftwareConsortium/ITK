// This is vcl/tests/test_map.cxx
#include <vcl_functional.h>
#include <vcl_iostream.h>
#include <vcl_map.h>

int test_map_main(int /*argc*/,char* /*argv*/[])
{
  bool okay = true;
  typedef vcl_map<int, double, vcl_less<int> > mymap;
  mymap m;

  m.insert(mymap::value_type(1, 2718));
  m.insert(mymap::value_type(2, 3141));

  for (mymap::iterator p = m.begin(); p != m.end(); ++p)
    vcl_cout << (*p).first << " " << (*p).second << vcl_endl;

  mymap::iterator i = m.find(3);
  okay = okay && (i == m.end()); // not found (=OK)
  i = m.find(2);
  okay = okay && (i != m.end()) && ((*i).second == 3141);

  if ( okay )
    return 0;
  else
    return 1;
}
