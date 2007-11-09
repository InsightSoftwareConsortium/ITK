//:
//  \file
//  \author Kevin de Souza
//  \date 10 January 2007
//  \brief Program to test vnl_rotation_matrix() functions


#include <vcl_iostream.h>
// not used? #include <vcl_iomanip.h>
#include <vcl_limits.h>

#include <testlib/testlib_test.h>

#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_rotation_matrix.h>
#include <vnl/vnl_random.h>


//: Tolerance between doubles. This was inferred by trial and error.
// Could be derived mathematically?
const double dtol = 4*vcl_numeric_limits<double>::epsilon();


//: Local enum to indicate choice of x,y or z-axis.
enum CartAxis
{
  x_axis,
  y_axis,
  z_axis
};


//: Random number generator
vnl_random randgen(9667566);


//: Compute the 3x3 rotation matrix corresponding to a single Euler angle rotation
//  (i.e., a rotation of angle phi about one of the Cartesian axes).
//  By convention, a positive angle indicates a clockwise rotation about an
//  axis when viewing the axis from the origin towards the positive direction.
static void get_rotation_matrix_euler_angle(
  const double phi,
  const CartAxis axis,
  vnl_matrix_fixed<double, 3, 3>& R)
{
  R.set_identity();

  if (vcl_fabs(phi)<vcl_numeric_limits<double>::epsilon())
    return;

  double cos_phi = vcl_cos(phi);
  double sin_phi = vcl_sin(phi);

  switch (axis)
  {
   case x_axis:
    R[1][1] = cos_phi;
    R[1][2] = -sin_phi;
    R[2][1] = sin_phi;
    R[2][2] = cos_phi;
    break;

   case y_axis:  // NB This appears different to x and z but I think it's right!
    R[0][0] = cos_phi;
    R[0][2] = sin_phi;
    R[2][0] = -sin_phi;
    R[2][2] = cos_phi;
    break;

   case z_axis:
    R[0][0] = cos_phi;
    R[0][1] = -sin_phi;
    R[1][0] = sin_phi;
    R[1][1] = cos_phi;
    break;

   default:
    break;
  }
}


//: Test the function vnl_rotation_matrix() for a specified \a axis (inc. angle) and true answer \a M.
static bool calc_and_test_matrix(const vnl_vector<double>& axis,
                                 const vnl_matrix_fixed<double,3,3>& M)
{
  vnl_matrix<double> R = vnl_rotation_matrix(axis);

  // Check that rotation matrix is 3x3
  bool success = (3==R.rows() && 3==R.cols());
  if (!success) return false;

  vnl_matrix<double> D = R - M;
  double max_err = D.absolute_value_max();

  // Check that rotation matrix is correct within a tolerance
  success = success && (max_err<=dtol);

#ifndef NDEBUG
  if (max_err>dtol)
  {
    vcl_cout << "Warning: max_err=" << max_err
             << "  eps=" << vcl_numeric_limits<double>::epsilon()
             << vcl_endl;
  }
#endif

  return success;
}


//: Test for the special cases of Euler-angle rotations
// (i.e. rotations about a single Cartesian axis).
// Many trials are performed with randomly-chosen rotation angles.
static void test_euler_rotations()
{
  bool success = true;
  const unsigned ntrials=100;
  for (unsigned i=0; i<ntrials; ++i)
  {
    bool this_trial_ok = true;
    double ang = randgen.drand32(-4*vnl_math::pi, 4*vnl_math::pi);

    vnl_vector<double> axis(3); // The magnitude of this vector indicates the angle of rotation
    vnl_matrix_fixed<double,3,3> M; // True answer

    //--- rotations about x-axis ---
    get_rotation_matrix_euler_angle(ang, x_axis, M);
    axis[0]=1.0;  axis[1]=0.0;  axis[2]=0.0;
    axis *= ang;
    this_trial_ok = this_trial_ok && calc_and_test_matrix(axis, M);

    //--- rotations about y-axis ---
    get_rotation_matrix_euler_angle(ang, y_axis, M);
    axis[0]=0.0;  axis[1]=1.0;  axis[2]=0.0;
    axis *= ang;
    this_trial_ok = this_trial_ok && calc_and_test_matrix(axis, M);

    //--- rotations about z-axis ---
    get_rotation_matrix_euler_angle(ang, z_axis, M);
    axis[0]=0.0;  axis[1]=0.0;  axis[2]=1.0;
    axis *= ang;
    this_trial_ok = this_trial_ok && calc_and_test_matrix(axis, M);

    success = success && this_trial_ok;
  }

  TEST("test_euler_rotations() for many trials", success, true);
}


void test_rotation_matrix()
{
  test_euler_rotations();
}

TESTMAIN(test_rotation_matrix);
