#include <vcl_iostream.h>
#include <vcl_cmath.h>
#include <vcl_complex.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matops.h>
#include <vnl/vnl_sample.h>

// make a vector with random, complex entries :
static void fill_rand(vcl_complex<double> *b, vcl_complex<double> *e) {
  for (vcl_complex<double> *p=b; p<e; ++p)
    (*p) = vcl_complex<double>( vnl_sample_uniform(-1, +1), vnl_sample_uniform(-1, +1) );
}

// Driver
void test_complex() {
  {
    vcl_complex<double> a(-5), b(7,-1), c;
    c = a + b;
    c = a - b;
    c = a * b;
    c = a / b;
    a += b;
    a -= b;
    a *= b;
    a /= b;
  }

  {
    vnl_vector<vcl_complex<double> > a(5); fill_rand(a.begin(), a.end());
    vnl_vector<vcl_complex<double> > b(5); fill_rand(b.begin(), b.end());

    vcl_cout << "a=" << a << vcl_endl;
    vcl_cout << "b=" << b << vcl_endl;

    vcl_complex<double> i(0,1);

    vnl_test_assert("inner_product() conjugates correctly",
                    vcl_abs(inner_product(i*a,b)-i*inner_product(a,b))<1e-12 &&
                    vcl_abs(inner_product(a,i*b)+i*inner_product(a,b))<1e-12 );

    vnl_test_assert("dot_product() does not conjugate",
                    vcl_abs( dot_product(i*a,b)-i*dot_product(a,b) ) < 1e-12 &&
                    vcl_abs( dot_product(a,i*b)-i*dot_product(a,b) ) < 1e-12 );

    double norma=0;
    for (unsigned n=0; n<a.size(); ++n)
      norma += a[n].real()*a[n].real() + a[n].imag()*a[n].imag();
    norma = vcl_sqrt(norma);
    vnl_test_assert("correct magnitude", vcl_abs(norma-a.magnitude())<1e-12 );
  }
}

TESTMAIN(test_complex);
