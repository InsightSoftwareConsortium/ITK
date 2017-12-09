// This is core/vnl/algo/vnl_rnpoly_solve.h
#ifndef vnl_rnpoly_solve_h_
#define vnl_rnpoly_solve_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Solves for roots of system of real polynomials
// \author Marc Pollefeys, ESAT-VISICS, K.U.Leuven
// \date   12-Aug-1997
//
// \verbatim
//  Modifications
//   Oct.1999 - Peter Vanroose - implementation simplified through "cmplx" class for doing complex arithmetic.
//   May.2002 - Peter Vanroose - added operator*=(cmplx) and operator/=(cmplx)
//   Mar.2003 - Peter Vanroose - renamed M to M_, T to T_
//   Feb.2004 - Peter Vanroose - removed hard limits on dimensionality; this gets rid of M_ and T_;
//                               now using std::vector throughout instead of C arrays of fixed size
// \endverbatim

#include <vector>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vcl_compiler.h>
#include <vnl/algo/vnl_algo_export.h>

//: Solves for roots of system of real polynomials
//  Calculates all the roots of a system of N polynomials in N variables
//  through continuation.
//  Adapted from the  PARALLEL CONTINUATION algorithm, written by Darrell
//  Stam, 1991, and further improved by  Kriegman and Ponce, 1992.

class VNL_ALGO_EXPORT vnl_rnpoly_solve
{
  // Data Members--------------------------------------------------------------
  std::vector<vnl_real_npolynomial*> ps_;   // the input
  std::vector<vnl_vector<double>*> r_; // the output (real part)
  std::vector<vnl_vector<double>*> i_; // the output (imaginary part)

 public:

  // Constructor---------------------------------------------------------------

  //: The constructor already does all the calculations
  inline vnl_rnpoly_solve(std::vector<vnl_real_npolynomial*> const& ps)
    : ps_(ps) { compute(); }

  // Destructor----------------------------------------------------------------

 ~vnl_rnpoly_solve();

  // Operations----------------------------------------------------------------

  //: Array of real parts of roots
  inline std::vector<vnl_vector<double>*> real() { return r_; }

  //: Array of imaginary parts of roots
  inline std::vector<vnl_vector<double>*> imag() { return i_; }

  //: Return real roots only.
  //  Roots are real if the absolute value of their imaginary part is less than
  //  the optional argument tol, which defaults to 1e-12 [untested]
  std::vector<vnl_vector<double>*> realroots(double tol = 1e-12);

  // Computations--------------------------------------------------------------

 private:
  //: Compute roots using continuation algorithm.
  bool compute();

  void Read_Input(std::vector<unsigned int>& ideg,
                  std::vector<unsigned int>& terms,
                  std::vector<int>& polyn,
                  std::vector<double>& coeff);
};

#endif // vnl_rnpoly_solve_h_
