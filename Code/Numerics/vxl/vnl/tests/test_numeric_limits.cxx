#include <vcl_iostream.h>
#include <vnl/vnl_numeric_limits.h>

int test_numeric_limits(int, char**)
{
  vcl_cout << "dmax  = " << vnl_numeric_limits<double>::max() << vcl_endl;
  vcl_cout << "dmin  = " << vnl_numeric_limits<double>::min() << vcl_endl;
  vcl_cout << "deps  = " << vnl_numeric_limits<double>::epsilon() << vcl_endl;
  vcl_cout << "dnmin = " << vnl_numeric_limits<double>::denorm_min() << vcl_endl;
#ifndef __BORLANDC__  
    vcl_cout << "dnan  = " << vnl_numeric_limits<double>::quiet_NaN() << vcl_endl;
#else
    vcl_cout << "This compiler cannot print (double) NaN" << vcl_endl;
#endif    
  vcl_cout << "dinf  = " << vnl_numeric_limits<double>::infinity() << vcl_endl;
  vcl_cout << "dninf = " << -vnl_numeric_limits<double>::infinity() << vcl_endl;

  vcl_cout << "fmax  = " << vnl_numeric_limits<float>::max() << vcl_endl;
  vcl_cout << "fmin  = " << vnl_numeric_limits<float>::min() << vcl_endl;
  vcl_cout << "feps  = " << vnl_numeric_limits<float>::epsilon() << vcl_endl;
  vcl_cout << "fnmin = " << vnl_numeric_limits<float>::denorm_min() << vcl_endl;
#ifndef __BORLANDC__  
  vcl_cout << "fnan  = " << vnl_numeric_limits<float>::quiet_NaN() << vcl_endl;
#else
    vcl_cout << "This compiler cannot print (float) NaN" << vcl_endl;
#endif    
  vcl_cout << "finf  = " << vnl_numeric_limits<float>::infinity() << vcl_endl;
  vcl_cout << "fninf = " << -vnl_numeric_limits<float>::infinity() << vcl_endl;
  return 0;
}
