// This is core/vnl/vnl_rotation_matrix.cxx
#include <cmath>
#include "vnl_rotation_matrix.h"

#include <vcl_compiler.h>

bool vnl_rotation_matrix(double const x[3], double **R)
{
  // start with an identity matrix.
  for (unsigned i=0; i<3; ++i)
    for (unsigned j=0; j<3; ++j)
      R[i][j] = (i==j ? 1 : 0);

  // normalize x to a unit vector u, of norm 'angle'.
  double u[3] = {x[0], x[1], x[2]};
  double angle = std::sqrt(u[0]*u[0] + u[1]*u[1] + u[2]*u[2]);
  if (angle == 0)
    return true;
  u[0] /= angle;
  u[1] /= angle;
  u[2] /= angle;

  // add (cos(angle)-1)*(1 - u u').
  double cos_angle = std::cos(angle);
  for (unsigned i=0; i<3; ++i)
    for (unsigned j=0; j<3; ++j)
      R[i][j] += (cos_angle-1) * ((i==j ? 1:0) - u[i]*u[j]);

  // add sin(angle) * [u]
  double sin_angle = std::sin(angle);
  /* */                      R[0][1] -= sin_angle*u[2]; R[0][2] += sin_angle*u[1];
  R[1][0] += sin_angle*u[2]; /* */                      R[1][2] -= sin_angle*u[0];
  R[2][0] -= sin_angle*u[1]; R[2][1] += sin_angle*u[0]; /* */

  return true;
}

bool vnl_rotation_matrix(double const axis[3], double R[3][3])
{
  double *R_[3] = { R[0], R[1], R[2] };
  return vnl_rotation_matrix(axis, R_);
}

bool vnl_rotation_matrix(double const axis[3], double *R0, double *R1, double *R2)
{
  double *R[3] = { R0, R1, R2 };
  return vnl_rotation_matrix(axis, R);
}

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

bool vnl_rotation_matrix(vnl_vector_fixed<double,3> const& axis,
                         vnl_matrix_fixed<double,3,3>& R)
{
  return vnl_rotation_matrix(&axis[0], R[0], R[1], R[2]);
}

vnl_matrix_fixed<double,3,3> vnl_rotation_matrix(vnl_vector_fixed<double,3> const& axis)
{
  vnl_matrix_fixed<double,3,3> R;
  vnl_rotation_matrix(&axis[0], R[0], R[1], R[2]);
  return R;
}

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

bool vnl_rotation_matrix(vnl_vector<double> const &axis, vnl_matrix<double> &R)
{
  return vnl_rotation_matrix(&axis[0], R.data_array());
}

vnl_matrix<double> vnl_rotation_matrix(vnl_vector<double> const &axis)
{
  vnl_matrix<double> R(3, 3);
  vnl_rotation_matrix(&axis[0], R.data_array());
  return R;
}
