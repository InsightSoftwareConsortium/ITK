/*
  fsm
*/
#include "vnl_cpoly_roots.h"
#include <vcl_cassert.h>
#include <vnl/algo/vnl_complex_eigensystem.h>

void vnl_cpoly_roots::compute(vnl_vector<std::complex<double> > const &a)
{
  // construct companion matrix
  vnl_matrix<std::complex<double> > comp(N, N);
  comp.fill(0);
  for (unsigned i=0; i<N-1; ++i)
    comp(i+1, i) = 1;
  for (unsigned i=0; i<N; ++i)
    comp(i, N-1) = -a[N-1-i];

  // the eigenvalues of the companion matrix are the roots of the polynomial
  solns = vnl_complex_eigensystem(comp,
                                  false,    // we only want
                                  false).W; // the eigenvalues.
#ifdef DEBUG
  std::cerr << "s = " << solns << '\n';
#endif
}

vnl_cpoly_roots::vnl_cpoly_roots(vnl_vector<std::complex<double> > const & a)
  : solns(a.size())
  , N(a.size()) // degree
{
  compute(a);
}

vnl_cpoly_roots::vnl_cpoly_roots(vnl_vector<double> const & a_real,
                                 vnl_vector<double> const & a_imag)
  : solns(a_real.size())
  , N(a_real.size()) // degree
{
  assert(a_real.size() == a_imag.size());
  vnl_vector<std::complex<double> > a(N);
  for (unsigned i=0; i<N; ++i)
    a[i] = std::complex<double>(a_real[i], a_imag[i]);

#ifdef DEBUG
  std::cerr << "a = " << a << '\n';
#endif
  compute(a);
}
