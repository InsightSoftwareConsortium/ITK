#ifndef vnl_cpoly_roots_h_
#define vnl_cpoly_roots_h_

// .NAME	vnl_cpoly_roots
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_cpoly_roots.h
// .FILE	vnl_cpoly_roots.cxx
//
// .SECTION Description
// Class to find all the roots of a univariate polynomial f
// with complex coefficients. Currently works by computing the 
// eigenvalues of the companion matrix of f.
//
// The input vector a of coefficients are given to the constructor.
// The polynomial is f = t^N + a[0] t^{N-1} + ... + a[N-1]
// The roots can then be found in the 'solns' member.
//
// .SECTION Author
//   fsm@robots.ox.ac.uk
//
#include <vnl/vnl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: find all the roots of a univariate polynomial with complex coefficients.
class vnl_cpoly_roots {
public:
  vnl_cpoly_roots(vnl_vector<vnl_double_complex> const & a);
  vnl_cpoly_roots(vnl_vector<double> const & a_real,
		  vnl_vector<double> const & a_imag);
  
  // the roots can be found in here :
  vnl_vector<vnl_double_complex> solns;

private:
  unsigned N; //degree
  void compute(vnl_vector<vnl_double_complex> const & a); // this does the actual work
};

#endif // vnl_cpoly_roots_h_
