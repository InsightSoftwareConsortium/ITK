#include <vcl_functional.h>
#include <vcl_utility.h>
#include <vcl_iostream.h>
#include <vcl_map.h>

typedef vcl_multimap<int, double, vcl_less<int> > mymap;

vcl_ostream &operator<<(vcl_ostream &s, mymap::value_type const &x) 
{
  return s << '(' << x.first << ',' << x.second << ')';
}

int main()
{
  mymap m;
  m.insert(mymap::value_type(0, 2.71828182845905));
  m.insert(mymap::value_type(2, 3.14159265358979));
  m.insert(mymap::value_type(2, 1.41421356237310));
  m.insert(mymap::value_type(3, 1.61803398874989));

  mymap::iterator b = m.begin();
  mymap::iterator e = m.end();

  vcl_cout << "the whole container:" << vcl_endl;
  for (mymap::iterator p = b; p != e; ++p)
    vcl_cout << *p << vcl_endl;
  
  vcl_cout << "lower_bound() and upper_bound():" << vcl_endl;
  for (int k=-1; k<=4; ++k) {  
    vcl_cout << "k=" << k << vcl_endl;

    mymap::iterator lo = m.lower_bound(k);
    vcl_cout << "  lo: ";
    if (lo==b) vcl_cout << "begin";
    else if (lo==e) vcl_cout << "end";
    else vcl_cout << *lo;
    vcl_cout << vcl_endl;

    mymap::iterator hi = m.upper_bound(k);
    vcl_cout << "  hi: ";
    if (hi==b) vcl_cout << "begin";
    else if (hi==e) vcl_cout << "end";
    else vcl_cout << *hi;
    vcl_cout << vcl_endl;
  }
  
  return 0;
}
