#include <vcl_iostream.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex.h>

int test_math(int,char* [] ) {
  vcl_complex<double> z(1,2);
  
  vcl_cout << "z=" << z << vcl_endl;
  vcl_cout << "abs(z)=" << vnl_math_abs(z) << vcl_endl;
  
  return 0;
}
