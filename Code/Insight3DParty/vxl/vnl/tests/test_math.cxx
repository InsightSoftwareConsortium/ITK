/*
  fsm@robots.ox.ac.uk
*/

#include <vcl_iostream.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex.h>

int main(int,char**) {
  vnl_double_complex z(1,2);
  
  vcl_cout << "z=" << z << vcl_endl;
  vcl_cout << "abs(z)=" << vnl_math_abs(z) << vcl_endl;
  
  return 0;
}
