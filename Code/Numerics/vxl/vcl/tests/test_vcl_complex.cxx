#include <vcl_complex_fwd.h>
vcl_complex<double> doublify(vcl_complex<float> const &);
  

#include <vcl_complex.h>
vcl_complex<double> doublify(vcl_complex<float> const &z) {
  return vcl_complex<double>(vcl_real(z), vcl_imag(z));
}

#include <vcl_iostream.h>

int main()
{
  vcl_complex<double> dc1(1.1,1.2), dc2(2.1,2.2);
  vcl_complex<float> fc1(1.1f,1.2f), fc2(2.1f,2.2f);

  vcl_cout << dc1 << " + " << dc2 << " = " << (dc1+dc2) << vcl_endl;
  vcl_cout << fc1 << " + " << fc2 << " = " << (fc1+fc2) << vcl_endl;

  vcl_complex<double> dc3(vcl_real(dc1),vcl_imag(dc2));
  vcl_complex<float> fc3(vcl_real(fc1),vcl_imag(fc2));
  
  vcl_cout << dc3 << " / " << dc1 << " = " << dc3/dc1 << vcl_endl;
  vcl_cout << fc3 << " / " << fc1 << " = " << fc3/fc1 << vcl_endl;
  
  vcl_cout << "polar representation of " << dc3 << " is [" << vcl_abs(dc3) << "," << vcl_arg(dc3) << ']' << vcl_endl;
  vcl_cout << "going back: " << dc3 << " must be = " << vcl_polar(vcl_abs(dc3), vcl_arg(dc3)) << vcl_endl;
  vcl_complex<float> fcsr3 = vcl_sqrt(fc3);
  vcl_cout << "sqrt(" << fc3 << ") is " << fcsr3 << ", so " << fcsr3 << " * " << fcsr3 << " = " << fcsr3*fcsr3 << vcl_endl;

  return 0;
}
