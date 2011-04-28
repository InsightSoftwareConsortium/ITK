// This little program computes the two complex square roots of 3+4i,
// by solving the complex quadratic equation x^2 - c = 0, with c=3+4i.
// It then also computes the four complex 4th roots of 24i-7 by solving
// x^4 - c4 = 0.

#include <vnl/algo/vnl_cpoly_roots.h>
#include <vcl_iostream.h>

int main()
{
  vcl_complex<double> c(3.0,4.0);
  vnl_vector<vcl_complex<double> > equation(2);
  // although the equation has three coefficients (1 for x^2, 0 for x and -c
  // as the constant coefficient), the highest order coefficient must always
  // be 1, and should not be placed in the equation vector.
  // next, equation[0] is the second highest coefficient, etc.:
  equation[0] = 0; equation[1] = -c;
  vnl_cpoly_roots r(equation);
  vcl_cout << "One square root of 3+4i is " << r.solns[0]
           << "\nThe other square root is " << r.solns[1] << vcl_endl;

  vcl_complex<double> c4(-7.0,24.0);
  vnl_vector<vcl_complex<double> > eq(4);
  eq[0] = eq[1] = eq[2] = 0; eq[3] = -c4;
  vnl_cpoly_roots r4(eq);
  vcl_cout << "\nThe 4th roots of 24i-7 are " << r4.solns[0]
           << r4.solns[1] << r4.solns[2] << r4.solns[3] << vcl_endl;

  return 0;
}
