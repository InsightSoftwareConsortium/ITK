// now #include the definition of vcl_complex<>
// and define the doublify() function.
#include <vcl_complex.h>
vcl_complex<double> doublify(vcl_complex<float> const &z)
{
  return vcl_complex<double>(vcl_real(z), vcl_imag(z));
}

#include <vcl_iostream.h>

int test_complex_main(int /*argc*/,char* /*argv*/[])
{
  vcl_complex<double> dc1(1.1,1.2), dc2(2.1,2.2);
  vcl_complex<float> fc1(1.1f,1.2f), fc2(2.1f,2.2f);

  vcl_cout << dc1 << " + " << dc2 << " = " << (dc1+dc2) << vcl_endl
           << fc1 << " + " << fc2 << " = " << (fc1+fc2) << vcl_endl;

  vcl_complex<double> dc3(vcl_real(dc1),vcl_imag(dc2));
  vcl_complex<float> fc3(vcl_real(fc1),vcl_imag(fc2));

  vcl_cout << dc3 << " / " << dc1 << " = " << dc3/dc1 << vcl_endl
           << fc3 << " / " << fc1 << " = " << fc3/fc1 << vcl_endl;

  vcl_cout << "polar representation of " << dc3 << " is [" << vcl_abs(dc3) << "," << vcl_arg(dc3) << ']' << vcl_endl
           << "going back: " << dc3 << " must be = " << vcl_polar(vcl_abs(dc3), vcl_arg(dc3)) << vcl_endl;
  vcl_complex<float> fcsr3 = vcl_sqrt(fc3);
  vcl_cout << "sqrt(" << fc3 << ") is " << fcsr3 << ", so " << fcsr3 << " * " << fcsr3 << " = " << fcsr3*fcsr3 << vcl_endl;

  // Should also test form of complex stream input and output. The standard says:
  // [26.2.6.12] operator>> understands "u", "(u)" and  "(u,v)" where u, v are real.
  // [26.2.6.13] operator<< does f << '(' << x.real() << "," << x.imag() << ')';
  // In particular, complex numbers written with operator<< can be read again with
  // operator>>.

  // complex should have a type called value_type;
  vcl_complex<float>::value_type tmp = 1.0f;
  tmp += 2.0f; // to avoid unused variable warnings.

  return 0;
}
