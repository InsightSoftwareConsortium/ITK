#include <vcl_iostream.h>
#include <vcl_deque.h>

int test_deque_main(int /*argc*/,char* /*argv*/[])
{
  typedef vcl_deque<int> mydeque;
  mydeque dq;

  dq.push_front(2);
  dq.push_back(3);
  dq.push_front(1);

  for (mydeque::iterator p = dq.begin(); p != dq.end(); ++p)
    vcl_cout << *p << vcl_endl;

  return 0;
}
