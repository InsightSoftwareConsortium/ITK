#include <vnl/vnl_test.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_real_polynomial.h>
#include <vnl/algo/vnl_cpoly_roots.h>

void testvnl_cpoly_roots()
{
  const double coeffs[] = {6, 5, 4, 3, 2, 1};
  vnl_vector<double> a(coeffs, 6);
  
  vnl_vector<double> monic( (a/a[0]).extract(a.size()-1,1) );
  vnl_cpoly_roots roots( monic, 0.0*monic );
  
  Assert( "Number of solutions", roots.solns.size() == monic.size() );
  
  // Evaluate results
  vnl_real_polynomial f(a);
  for(int i = 0; i < f.degree(); ++i)
    Assert("Root residual", vnl_math_abs(f.evaluate(roots.solns[i])) < 1e-12);
}

TESTMAIN(testvnl_cpoly_roots);
