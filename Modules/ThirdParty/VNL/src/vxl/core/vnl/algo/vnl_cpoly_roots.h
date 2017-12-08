#ifndef vnl_cpoly_roots_h_
#define vnl_cpoly_roots_h_

//:
//  \file
//  \brief  finds roots of a univariate polynomial with complex coefficients
//  \author fsm
//
// \verbatim
// Modifications
//  dac (Manchester) March 28th 2001: Tidied documentation
// \endverbatim

#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_algo_export.h>

//: Find all the roots of a univariate polynomial with complex coefficients.
//  Class to find all the roots of a univariate polynomial f
//  with complex coefficients. Currently works by computing the
//  eigenvalues of the companion matrix of f.
//
//  The input vector a of coefficients are given to the constructor.
//  The polynomial is f = t^N + a[0] t^{N-1} + ... + a[N-1]
//  The roots can then be found in the 'solns' member.

class VNL_ALGO_EXPORT vnl_cpoly_roots
{
public:
  vnl_cpoly_roots(vnl_vector<std::complex<double> > const & a);
  vnl_cpoly_roots(vnl_vector<double> const & a_real,
                  vnl_vector<double> const & a_imag);

  // the roots can be found in here :
  vnl_vector<std::complex<double> > solns;

private:
  unsigned N; //degree
  //: does the actual work
  void compute(vnl_vector<std::complex<double> > const & a);
};

#endif // vnl_cpoly_roots_h_
