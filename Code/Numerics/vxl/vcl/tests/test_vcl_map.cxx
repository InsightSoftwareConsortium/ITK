#include <vcl_functional.h>
#include <vcl_utility.h>
#include <vcl_iostream.h>
#include <vcl_map.h>

int main()
{
  typedef vcl_map<int, double, vcl_less<int> > mymap;
  mymap m;
  
  m.insert(mymap::value_type(1, 2718));
  m.insert(mymap::value_type(2, 3141));

  for (mymap::iterator p = m.begin(); p != m.end(); ++p)
    vcl_cout << (*p).first << " " << (*p).second << vcl_endl;

  mymap::iterator i = m.find(3);

  return 0;
}
