/*
  fsm
*/
struct Y
{
  int x;
  Y(int x_) : x(x_) { }
  bool operator==(Y const &that) const { return x == that.x; }
  bool operator< (Y const &that) const { return x <  that.x; }
};

#include <vcl_rel_ops.h>

int function()
{
  Y x(2), y(3);
  if (x == y) return 1;
  if (x != y) return 2;
  if (x <  y) return 3;
  if (x >  y) return 4;
  if (x <= y) return 5;
  if (x >= y) return 6;
  return 0;
}

int test_rel_ops_main(int /*argc*/,char* /*argv*/[])
{
  function();
  return 0;
}

VCL_REL_OPS_INSTANTIATE(Y);
