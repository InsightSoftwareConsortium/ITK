#ifndef vnl_complex_eigensystem_h_
#define vnl_complex_eigensystem_h_
//
// .NAME	vnl_complex_eigensystem
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_complex_eigensystem.h
// .FILE	vnl_complex_eigensystem.cxx
//
// .SECTION Description
// Class to compute and hold the eigenvalues and (optionally) eigenvectors
// of a square complex matrix, using the LAPACK routine zgeev.
//
// Default behaviour is to compute the eigenvalues and the right
// eigenvectors.
//
// The input NxN matrix A is passed into the constructor. The flags
// right,left request the calculation of right and left eigenvectors
// respectively. The compute eigenvalues are stored in the member 'W'.
//
// Computed right eigenvectors are stored in the **ROWS** of the
// member 'R' and computed left eigenvectors are stored in the **ROWS**
// of the member 'L'. When eigenvectors are not requested, the corre-
// sponding matrices L and R will be empty.
//
// The ith right eigenvector v satisfies A*v = W[i]*v
// The ith left  eigenvector u satisfies u*A = W[i]*u (no conjugation)
//
// .SECTION Author
//  fsm@robots.ox.ac.uk
//
#include <vnl/vnl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: computes the eigenvalues [+eigenvectors] of a complex square matrix.
class vnl_complex_eigensystem {
public:

  vnl_complex_eigensystem(const vnl_matrix<double> &A_real,const vnl_matrix<double> &A_imag,
		     bool right=true,bool left=false);

  vnl_complex_eigensystem(const vnl_matrix<vnl_double_complex> &A,
		     bool right=true,bool left=false);

  ~vnl_complex_eigensystem();

  // please do not add underscores to my members.
  int const N;
  vnl_matrix<vnl_double_complex> L; // left evecs
  vnl_matrix<vnl_double_complex> R; // right evecs
  vnl_vector<vnl_double_complex> W; // evals

  // convenience methods
  vnl_double_complex eigen_value(unsigned i) const { return W[i]; }
  vnl_vector<vnl_double_complex> left_eigen_vector(unsigned i) const { return L.get_row(i); }
  vnl_vector<vnl_double_complex> right_eigen_vector(unsigned i) const { return R.get_row(i); }

private:
  void compute(const vnl_matrix<vnl_double_complex> &,bool,bool);
};

#endif // vnl_complex_eigensystem_h_
