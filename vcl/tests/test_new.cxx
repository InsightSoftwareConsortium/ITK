/*
  fsm
*/
#include <vcl_new.h>

struct X_s
{
  double *p;
  X_s() { p = new double[37]; }
  ~X_s() { delete [] p; }
};

int test_new_main(int /*argc*/,char* /*argv*/[])
{
  X_s my_x;

  vcl_destroy(&my_x);
  new (&my_x) X_s; // vcl_construct(&my_x);

  return 0;
}
