#ifndef vnl_complex_eigensystem_h_
#define vnl_complex_eigensystem_h_

//:
//  \file
//  \brief  Calculates eigenvalues and eigenvectors of a square complex matrix
//  \author fsm@robots.ox.ac.uk
//
//  \verbatim
//  Modifications
//  dac (Manchester) 26/03/2001: tidied up documentation
//  \endverbatim
//

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>


//: Calculates eigenvalues and eigenvectors of a square complex matrix
//
//  Class to compute and hold the eigenvalues and (optionally) eigenvectors
//  of a square complex matrix, using the LAPACK routine zgeev.
//
//  Default behaviour is to compute the eigenvalues and the right
//  eigenvectors.
//
//  The input NxN matrix A is passed into the constructor. The flags
//  right,left request the calculation of right and left eigenvectors
//  respectively. The compute eigenvalues are stored in the member 'W'.
//
//  Computed right eigenvectors are stored in the **ROWS** of the
//  member 'R' and computed left eigenvectors are stored in the **ROWS**
//  of the member 'L'. When eigenvectors are not requested, the corre-
//  sponding matrices L and R will be empty.
//
//  The ith right eigenvector v satisfies A*v = W[i]*v
//  The ith left  eigenvector u satisfies u*A = W[i]*u (no conjugation)
//

class vnl_complex_eigensystem {
public:

  vnl_complex_eigensystem(const vnl_matrix<double> &A_real,
                          const vnl_matrix<double> &A_imag,
                          bool right=true, bool left=false);

  vnl_complex_eigensystem(const vnl_matrix<vcl_complex<double> > &A,
                          bool right=true, bool left=false);

  ~vnl_complex_eigensystem();

  // please do not add underscores to my members.
  int const N;
  vnl_matrix<vcl_complex<double> > L; // left evecs
  vnl_matrix<vcl_complex<double> > R; // right evecs
  vnl_vector<vcl_complex<double> > W; // evals

  // convenience methods
  vcl_complex<double> eigen_value(unsigned i) const { return W[i]; }
  vnl_vector<vcl_complex<double> > left_eigen_vector(unsigned i)
      const { return L.get_row(i); }
  vnl_vector<vcl_complex<double> > right_eigen_vector(unsigned i)
      const { return R.get_row(i); }

private:
  void compute(const vnl_matrix<vcl_complex<double> > &,bool,bool);
};

#endif // vnl_complex_eigensystem_h_
