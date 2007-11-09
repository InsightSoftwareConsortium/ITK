#include <vcl_iostream.h>
#include <vcl_functional.h>
#include <vcl_set.h>

int test_set_main(int /*argc*/,char* /*argv*/[])
{
  typedef vcl_set<int, vcl_less<int> > myset;
  myset s;

  s.insert(1);

  for (myset::iterator p = s.begin(); p != s.end(); ++p)
    vcl_cout << *p << vcl_endl;
  return 0;
}
