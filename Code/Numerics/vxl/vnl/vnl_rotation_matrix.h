#ifndef vnl_rotation_matrix_h_
#define vnl_rotation_matrix_h_
// .NAME vnl_rotation_matrix
// .HEADER vxl package
// .LIBRARY vnl
// .INCLUDE vnl/vnl_rotation_matrix.h
// .FILE vnl_rotation_matrix.cxx

template <class T> class vnl_vector;
template <class T> class vnl_matrix;
//#include <vnl/vnl_vector.h>
//#include <vnl/vnl_matrix.h>

// Functions to compute the exponential of a skew 3x3 matrix [x].
// The result is a (special) orthogonal 3x3 matrix which is a
// rotation about the axis x, by an angle equal to ||x||.

bool vnl_rotation_matrix(double const axis[3], double **R);
bool vnl_rotation_matrix(double const axis[3], double *R0, double *R1, double *R2);
bool vnl_rotation_matrix(double const axis[3], double R[3][3]);
bool vnl_rotation_matrix(vnl_vector<double> const &axis, vnl_matrix<double> &R);

vnl_matrix<double> vnl_rotation_matrix(vnl_vector<double> const &axis);

#endif // vnl_rotation_matrix_h_
