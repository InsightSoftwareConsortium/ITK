// This is vcl/tests/test_stlfwd.cxx
#include <iostream>
#include <functional>
#include <string>
#include <map>
#include <set>
#include <list>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

void f(std::map<int, std::string,std::less<int> >*,
       std::set<int,std::less<int> >*,
       std::list<int>*
      )
{
}


int test_stlfwd_main(int /*argc*/,char* /*argv*/[])
{
  return 0;
}
