#include <vcl_compiler.h> // for vcl_long_double

vcl_long_double function(vcl_long_double a)
{
  vcl_long_double b = (1 - a)*(1 + a + a*a);
  return b;
}

int main()
{
  vcl_long_double a = 2.0;
  vcl_long_double b = function(a);
  a = (1 - a*a*a) / b;
  return 0;
}
