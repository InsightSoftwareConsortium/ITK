// This is vxl/vnl/vnl_rotation_matrix.h
#ifndef vnl_rotation_matrix_h_
#define vnl_rotation_matrix_h_
//:
//  \file
//  \brief Functions to create a 3x3 rotation matrix
//
// The result is a (special) orthogonal 3x3 matrix which is a
// rotation about the axis, by an angle equal to ||axis||.

template <class T> class vnl_vector;
template <class T> class vnl_matrix;

bool vnl_rotation_matrix(double const axis[3], double **R);
bool vnl_rotation_matrix(double const axis[3], double *R0, double *R1, double *R2);
bool vnl_rotation_matrix(double const axis[3], double R[3][3]);
bool vnl_rotation_matrix(vnl_vector<double> const &axis, vnl_matrix<double> &R);

//: Returns an orthogonal 3x3 matrix which is a rotation about the axis, by an angle equal to ||axis||.
// \relates vnl_matrix
vnl_matrix<double> vnl_rotation_matrix(vnl_vector<double> const &axis);

#endif // vnl_rotation_matrix_h_
