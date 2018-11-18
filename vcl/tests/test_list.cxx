
#include <iostream>
#include <list>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

int test_list_main(int /*argc*/,char* /*argv*/[])
{
  typedef std::list<int> container;
  container m;

  m.push_back(1);
  m.push_back(2);

  for (const container::value_type & p : m)
    std::cout << p << std::endl;

  // fixme how do i do this on win32?  copy(m.begin(), m.end(), ostream_iterator<int>(cerr));
  return 0;
}
