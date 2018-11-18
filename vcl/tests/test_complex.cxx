// now #include the definition of std::complex<>
// and define the doublify() function.


#include <iostream>
#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif


std::complex<double> doublify(std::complex<float> const &z)
{
  return std::complex<double>(std::real(z), std::imag(z));
}


int test_complex_main(int /*argc*/,char* /*argv*/[])
{
  std::complex<double> dc1(1.1,1.2), dc2(2.1,2.2);
  std::complex<float> fc1(1.1f,1.2f), fc2(2.1f,2.2f);

  std::cout << dc1 << " + " << dc2 << " = " << (dc1+dc2) << std::endl
           << fc1 << " + " << fc2 << " = " << (fc1+fc2) << std::endl;

  std::complex<double> dc3(std::real(dc1),std::imag(dc2));
  std::complex<float> fc3(std::real(fc1),std::imag(fc2));

  std::cout << dc3 << " / " << dc1 << " = " << dc3/dc1 << std::endl
           << fc3 << " / " << fc1 << " = " << fc3/fc1 << std::endl;

  std::cout << "polar representation of " << dc3 << " is [" << std::abs(dc3) << ',' << std::arg(dc3) << "]\n"
           << "going back: " << dc3 << " must be = " << std::polar(std::abs(dc3), std::arg(dc3)) << std::endl;
  std::complex<float> fcsr3 = std::sqrt(fc3);
  std::cout << "sqrt(" << fc3 << ") is " << fcsr3 << ", so " << fcsr3 << " * " << fcsr3 << " = " << fcsr3*fcsr3 << std::endl;

  // Should also test form of complex stream input and output. The standard says:
  // [26.2.6.12] operator>> understands "u", "(u)" and  "(u,v)" where u, v are real.
  // [26.2.6.13] operator<< does f << '(' << x.real() << ',' << x.imag() << ')';
  // In particular, complex numbers written with operator<< can be read again with
  // operator>>.

  // complex should have a type called value_type;
  std::complex<float>::value_type tmp = 1.0f;
  (void)tmp; // to avoid unused variable warnings.


  // Test the std::pow functions

  bool success = true;

  const std::complex<double> neg1(-1.0, 0.0);
  const std::complex<double> i(0.0,1.0);
  std::complex<double> sqrt_neg1 = std::pow(neg1, 0.5);
  std::cout << "pow("<<neg1<<",0.5) = "<<sqrt_neg1<<
    " and should be (0,1)"<<std::endl;
  double error = std::abs(sqrt_neg1-i);
// need to be careful of quiet NANs
  if ( error < 0.0  || 1e-6 < error)
  {
    std::cout << "** FAILURE **\n";
    success = false;
  }

  const std::complex<double> half(0.5,0.0);
  sqrt_neg1 = std::pow(neg1, half);
  std::cout << "pow("<<neg1<<','<<half<<") = "<<sqrt_neg1<<
    " and should be (0,1)"<<std::endl;
  error = std::abs(sqrt_neg1-i);
  if ( error < 0.0  || 1e-6 < error)
  {
    std::cout << "** FAILURE **\n";
    success = false;
  }

  std::complex<double> zero(0.0,0.0);
  std::cout << "Implementation defines std::pow((0,0),(0,0)) = "
           << std::pow(zero, zero) << std::endl;

  {
    std::complex<double> x(2, 3);
    std::complex<double> xc = std::conj(x);
    std::cout << "Conjugate " << x << " = " << xc << '\n';
    if ( xc != std::complex<double>(2,-3) ) {
      success = false;
    }
  }

  return success?0:1;
}
