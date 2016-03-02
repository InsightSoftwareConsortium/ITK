
#include <vcl_iostream.h>
#include <vcl_list.h>

int test_list_main(int /*argc*/,char* /*argv*/[])
{
  typedef vcl_list<int> container;
  container m;

  m.push_back(1);
  m.push_back(2);

  for (container::iterator p = m.begin(); p != m.end(); ++p)
    vcl_cout << (*p) << vcl_endl;


  // fixme how do i do this on win32?  copy(m.begin(), m.end(), ostream_iterator<int>(cerr));

  return 0;
}
