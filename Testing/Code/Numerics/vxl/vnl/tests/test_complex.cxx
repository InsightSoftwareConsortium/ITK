#include <vcl_iostream.h>

#include <vnl/vnl_complex.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matops.h>
#include <vnl/vnl_sample.h>

// make a vector with random, complex entries :
static void fill_rand(vnl_double_complex *b, vnl_double_complex *e) {
  for (vnl_double_complex *p=b; p<e; ++p)
    (*p) = vnl_double_complex( vnl_sample_uniform(-1, +1), vnl_sample_uniform(-1, +1) );
}

// Driver
void test_complex() {
  {
    vnl_double_complex a(-5), b(7,-1), c;
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
    vnl_vector<vnl_double_complex> a(5); fill_rand(a.begin(), a.end());
    vnl_vector<vnl_double_complex> b(5); fill_rand(b.begin(), b.end());
    
    vcl_cout << "a=" << a << vcl_endl;
    vcl_cout << "b=" << b << vcl_endl;
    
    vnl_double_complex i(0,1);
    
    Assert("inner_product() conjugates correctly", 
	   vnl_math_abs( inner_product(i*a,b)-i*inner_product(a,b) ) < 1e-12 &&
	   vnl_math_abs( inner_product(a,i*b)+i*inner_product(a,b) ) < 1e-12 );
    
    Assert("dot_product() does not conjugate", 
	   vnl_math_abs( dot_product(i*a,b)-i*dot_product(a,b) ) < 1e-12 &&
	   vnl_math_abs( dot_product(a,i*b)-i*dot_product(a,b) ) < 1e-12 );
    
    double norma=0;
    for (unsigned n=0; n<a.size(); ++n)
      norma += a[n].real()*a[n].real() + a[n].imag()*a[n].imag();
    norma = sqrt(norma);
    Assert("correct magnitude", vnl_math_abs( norma-a.magnitude() ) < 1e-12 );
  }
}

TESTMAIN(test_complex);
