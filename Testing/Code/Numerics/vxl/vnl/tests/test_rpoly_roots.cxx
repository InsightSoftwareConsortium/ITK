#include <vnl/vnl_test.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_real_polynomial.h>
#include <vnl/algo/vnl_rpoly_roots.h>

void testvnl_rpoly_roots()
{
  double coeffs[] = {5, 4, 3, 2, 1};
  vnl_vector<double> a(coeffs, 5);
  
  vnl_rpoly_roots roots(a);
  
  Assert("Result sizes", (roots.real().size() == 4) && (roots.imag().size() == 4));
  Assert("Complex size", (roots.roots().size() == 4));
  //Assert("degree", roots.degree() == 4);

  // Evaluate results
  vnl_real_polynomial p(a);
  for(int i = 0; i < p.degree(); ++i)
    Assert("Root residual", vnl_math_abs(p.evaluate(roots[i])) < 1e-12);
}

TESTMAIN(testvnl_rpoly_roots);
