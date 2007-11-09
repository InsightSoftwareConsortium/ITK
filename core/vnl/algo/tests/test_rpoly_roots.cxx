#include <vnl/vnl_real_polynomial.h>
#include <vnl/algo/vnl_rpoly_roots.h>

#include <testlib/testlib_test.h>

void test_rpoly_roots()
{
  double coeffs[] = {5, 4, 3, 2, 1};
  vnl_vector<double> a(coeffs, 5);

  vnl_rpoly_roots roots(a);

  testlib_test_assert("Result sizes", (roots.real().size() == 4) && (roots.imag().size() == 4));
  testlib_test_assert("Complex size", (roots.roots().size() == 4));
  //testlib_test_assert("degree", roots.degree() == 4);

  // Evaluate results
  vnl_real_polynomial p(a);
  for (int i = 0; i < p.degree(); ++i)
    testlib_test_assert("Root residual", vcl_abs(p.evaluate(roots[i])) < 1e-12);
}

TESTMAIN(test_rpoly_roots);
