#ifndef vnl_rnpoly_solve_h_
#define vnl_rnpoly_solve_h_

//:
//  \file
//  \brief Solves for roots of system of real polynomials
//  \author Marc Pollefeys, ESAT-VISICS, K.U.Leuven, 12-08-97
//
//  Modifications
//  Peter Vanroose, 20 Oct 1999: implementation simplified through "cmplx"
//                                 class for doing complex arithmetic.
//  dac (Manchester) 28/03/2001: tidied up documentation
//

#include <vnl/vnl_vector.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vcl_vector.h>

//: Solves for roots of system of real polynomials
//  Calculates all the roots of a system of N polynomials in N variables
//  through continuation.
//  Adapted from the  PARALLEL CONTINUATION algorithm , written by Darrell
//  Stam, 1991, and further improved by  Kriegman and Ponce, 1992.
//


#ifdef static
# error "grr!!"
#endif

//: Solves for roots of system of real polynomials

class vnl_rnpoly_solve {
public:
#ifndef _WIN32
  static const unsigned int M = 11;   // Maximum dimension of problem
  static const unsigned int T = 2500; // Max. number of terms in a polynomial
#else
#ifdef __BORLANDC__
  enum { M = 11 };   // Maximum dimension of problem
  enum { T = 800 }; // Maximum number of terms in a polynomial
#else
  enum { M = 11 };   // Maximum dimension of problem
  enum { T = 2500 }; // Maximum number of terms in a polynomial
#endif
#endif

  // Constructor---------------------------------------------------------------

  //: The constructor already does all the calculations
  inline vnl_rnpoly_solve(vcl_vector<vnl_real_npolynomial*> 
        const& ps) : ps_(ps) {compute();}
  ~vnl_rnpoly_solve();

  // Operations----------------------------------------------------------------

//: Array of real parts of roots
  inline vcl_vector<vnl_vector<double>*> real() { return r_; }

//: Array of imaginary parts of roots
  inline vcl_vector<vnl_vector<double>*> imag() { return i_; }

//: Return real roots only.  Roots are real if the absolute value
// of their imaginary part is less than the optional argument tol,
// which defaults to 1e-12 [untested]
  vcl_vector<vnl_vector<double>*> realroots(double tol = 1e-12);

  // Computations--------------------------------------------------------------

private:
  //: Compute roots using continuation algorithm.
  bool compute();

  int Read_Input(int ideg[M], int terms[M],
                 int polyn[M][T][M], double coeff[M][T]);

  // Data Members--------------------------------------------------------------
  vcl_vector<vnl_real_npolynomial*> ps_;   // the input
  vcl_vector<vnl_vector<double>*> r_; // the output (real part)
  vcl_vector<vnl_vector<double>*> i_; // the output (imaginary part)
};

#endif // vnl_rnpoly_solve_h_
