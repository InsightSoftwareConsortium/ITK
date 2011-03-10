// This is core/vnl/algo/vnl_rpoly_roots.h
#ifndef vnl_rpoly_roots_h_
#define vnl_rpoly_roots_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Finds roots of a real polynomial
//  \author  Andrew W. Fitzgibbon, Oxford RRG
//  \date   06 Aug 96
//
// \verbatim
//  Modifications
//  23 may 97, Peter Vanroose - "NO_COMPLEX" option added (until "complex" type is standardised)
//  dac (Manchester) 28/03/2001: tidied up documentation
//  Joris Van den Wyngaerd - June 2001 - impl for vnl_real_polynomial constr added
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//  \endverbatim

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>

class vnl_real_polynomial;

//: Find the roots of a real polynomial.
//  Uses algorithm 493 from
//  ACM Trans. Math. Software - the Jenkins-Traub algorithm, described
//  by Numerical Recipes under "Other sure-fire techniques" as
//  "practically a standard in black-box polynomial rootfinders".
//  (See M.A. Jenkins, ACM TOMS 1 (1975) pp. 178-189.).
//
//  This class is not very const-correct as it is intended as a compute object
//  rather than a data object.

class vnl_rpoly_roots
{
 public:
// Constructors/Destructors--------------------------------------------------

  //: The constructor calculates the roots.
  // This is the most efficient interface
  // as all the result variables are initialized to the correct size.
  // The polynomial is $ a[0] x^d + a[1] x^{d-1} + \cdots + a[d] = 0 $.
  //
  // Note that if the routine fails, not all roots will be found.  In this case,
  // the "realroots" and "roots" functions will return fewer than n roots.

  vnl_rpoly_roots(const vnl_vector<double>& a);

  //: Calculate roots of a vnl_real_polynomial. Same comments apply.
  vnl_rpoly_roots(const vnl_real_polynomial& poly);

  // Operations----------------------------------------------------------------

  //: Return i'th complex root
  vcl_complex<double> operator [] (int i) const { return vcl_complex<double>(r_[i], i_[i]); }

  //: Complex vector of all roots.
  vnl_vector<vcl_complex<double> > roots() const;

  //: Real part of root I.
  const double& real(int i) const { return r_[i]; }

  //: Imaginary part of root I.
  const double& imag(int i) const { return i_[i]; }

  //: Vector of real parts of roots
  vnl_vector<double>& real() { return r_; }

  //: Vector of imaginary parts of roots
  vnl_vector<double>& imag() { return i_; }

  //: Return real roots only.
  //  Roots are real if the absolute value of their imaginary part is less than
  //  the optional argument TOL. TOL defaults to 1e-12 [untested]
  vnl_vector<double> realroots(double tol = 1e-12) const;

  // Computations--------------------------------------------------------------

  //: Compute roots using Jenkins-Traub algorithm.
  bool compute();

  //: Compute roots using QR decomposition of companion matrix. [unimplemented]
  bool compute_qr();

  //: Compute roots using Laguerre algorithm. [unimplemented]
  bool compute_laguerre();

 protected:
  // Data Members--------------------------------------------------------------
  vnl_vector<double> coeffs_;

  vnl_vector<double> r_;
  vnl_vector<double> i_;

  int num_roots_found_;
};

#endif // vnl_rpoly_roots_h_
