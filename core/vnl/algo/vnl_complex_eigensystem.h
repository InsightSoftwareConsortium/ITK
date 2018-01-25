#ifndef vnl_complex_eigensystem_h_
#define vnl_complex_eigensystem_h_
//:
//  \file
//  \brief  Calculates eigenvalues and eigenvectors of a square complex matrix
//  \author fsm
//
//  \verbatim
//   Modifications
//    dac (Manchester) 26/03/2001: tidied up documentation
//  \endverbatim

#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

#include <vnl/algo/vnl_algo_export.h>

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
//  of the member 'L'. When eigenvectors are not requested, the
//  corresponding matrices L and R will be empty.
//
//  The ith right eigenvector v satisfies A*v = W[i]*v   \n
//  The ith left  eigenvector u satisfies u*A = W[i]*u (no conjugation)

class VNL_ALGO_EXPORT vnl_complex_eigensystem
{
 public:
  // please do not add underscores to my members - they are publicly accessible
  unsigned int const N;
  vnl_matrix<std::complex<double> > L; // left evecs
  vnl_matrix<std::complex<double> > R; // right evecs
  vnl_vector<std::complex<double> > W; // evals

  // constructors
  vnl_complex_eigensystem(vnl_matrix<double> const& A_real,
                          vnl_matrix<double> const& A_imag,
                          bool right=true, bool left=false);

  vnl_complex_eigensystem(vnl_matrix<std::complex<double> > const& A,
                          bool right=true, bool left=false);

  // convenience methods
  std::complex<double> eigen_value(unsigned i) const { return W[i]; }
  vnl_vector<std::complex<double> > left_eigen_vector(unsigned i)
      const { return L.get_row(i); }
  vnl_vector<std::complex<double> > right_eigen_vector(unsigned i)
      const { return R.get_row(i); }

 private:
  void compute(vnl_matrix<std::complex<double> > const&,bool,bool);
};

#endif // vnl_complex_eigensystem_h_
