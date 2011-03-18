// This is core/vnl/examples/vnl_sample_beta.cxx

//:
// \file
// \brief Generate a sample set from a symmetric beta distribution
//
// This program generates n samples drawn from a Beta(a,a) distribution
// for given a (first command line argument), and where n is the second argument.
//
// Algorithm is based on a theorem by Ulrich (1984) stating that, when U and V
// are independent uniform [0,1] random variables, then the following r.v. is
// Beta(a,a):
// $\frac12 + \frac12 \sin(2\pi V) \sqrt{1-U^{\frac2{2a-1}}}$.
// 
// \author Peter Vanroose, ABIS, Leuven, Belgium
// \date   November 2009
//-----------------------------------------------------------------------------

#include <vnl/vnl_sample.h>
#include <vnl/vnl_math.h>
#include <vcl_cmath.h>
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>

int main(int argc, char* argv[])
{
  if (argc != 3)
  {
    vcl_cout << "This program generates n samples drawn from a Beta(a,a) distribution\n"
             << "for given a (first command line argument), and where n is the second argument.\n";
    return -1;
  }
  double a = vcl_atof(argv[1]);
  int n = vcl_atoi(argv[2]);
  while (n--) {
    double u = vnl_sample_uniform(0,1);
    double v = vnl_sample_uniform(0,2*vnl_math::pi);
    vcl_cout << 0.5+0.5*vcl_sin(v)*vcl_sqrt(1.0-vcl_pow(u,1.0/(a-0.5))) << '\n';
  }
  return 0;
}
