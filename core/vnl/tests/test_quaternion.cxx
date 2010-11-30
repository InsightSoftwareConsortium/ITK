#include <vcl_iostream.h>
// not used? #include <vcl_iomanip.h>
#include <vcl_limits.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_quaternion.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_rotation_matrix.h>

// Tolerance between doubles. This was inferred by trial and error.
// Could be derived mathematically?
const double dtol = 16*vcl_numeric_limits<double>::epsilon();


static void test_operators()
{
  vnl_quaternion<double> a(0,0,0,1), b(2,2,2,2), c, d(2,2,2,3), e(1,2,3,4);
  TEST("!=", a!=b, true);
  TEST("==", a==a, true);
  c = a + b; TEST("+", c, d);
  TEST(".x()", e.x(), 1.0);
  TEST(".y()", e.y(), 2.0);
  TEST(".z()", e.z(), 3.0);
  TEST(".r()", e.r(), 4.0);
  vcl_cout << vcl_endl;
}


static void test_random_round_trip()
{
  vnl_random rng(13241ul);
  unsigned errcount=0;
  double avg_sqr_error = 0.0;
  for (unsigned i=0;i<1000;++i)
  {
    // Need to be careful abount wrap around - don't test with angles that are too big
    vnl_vector_fixed<double,3> euler(rng.normal()*vnl_math::pi/18.0,
                                     rng.normal()*vnl_math::pi/18.0,
                                     rng.normal()*vnl_math::pi/18.0);
    vnl_quaternion<double> quat(euler(0), euler(1), euler(2));
    vnl_vector_fixed<double,3> out = quat.rotation_euler_angles();
    double err = vnl_vector_ssd(euler, out);
    avg_sqr_error+=err;
    if (err > 1e-16)
    {
      errcount++;
      vcl_cout << "ERROR: " << euler << vcl_endl;
    }
  }
  TEST("1000*Random euler -> quaternion -> euler consistent", errcount, 0);
  vcl_cout << "Average squared error: " << avg_sqr_error << vcl_endl;
}

static void test_random_euler_near_zero()
{
  vnl_random rng(13241ul);
  unsigned errcount=0;
  double avg_sqr_error = 0.0;
  for (unsigned i=0;i<1000;++i)
  {
    // Need to be careful abount wrap around - don't test with angles that are too big
    vnl_vector_fixed<double,3> euler(rng.normal()*vnl_math::pi/180.0,
                                     rng.normal()*vnl_math::pi/180.0,
                                     rng.normal()*vnl_math::pi/180.0);
    vnl_quaternion<double> quat(euler(0), euler(1), euler(2));
    if (quat.angle() > vnl_math::pi/36.0)
    {
      errcount++;
      vcl_cout << "ERROR: should be small: " << euler << ": " << quat << vcl_endl;
    }
    quat *= -1.0;
    vnl_vector_fixed<double,3> out = quat.rotation_euler_angles();
    double err = vnl_vector_ssd(euler, out);
    avg_sqr_error+=err;
    if (err > 1e-16)
    {
      errcount++;
      vcl_cout << "ERROR: -quat -> euler == quat -> euler" << euler << ": " << out << vcl_endl;
    }
  }
  TEST("1000*Random small euler -> small quaternion angle", errcount, 0);
  vcl_cout << "Average squared error: " << avg_sqr_error << vcl_endl;
}

static void test_random_quat_near_zero()
{
  vnl_random rng(13241ul);
  unsigned errcount=0;
  for (unsigned i=0;i<1000;++i)
  {
    vnl_quaternion<double> quat(rng.normal()/1000.0, rng.normal()/1000.0, rng.normal()/1000.0,
                                vnl_math_sgn0(rng.normal()) * (1.0+rng.normal()/1000.0) );
    quat.normalize();

    vnl_vector_fixed<double,3> euler = quat.rotation_euler_angles();

    if (euler.magnitude() > 0.01)
    {
      errcount++;
      vcl_cout << "ERROR: should be small: " << quat << ": " << euler << vcl_endl;
    }
  }
  TEST("1000*Random small quat -> small euler values", errcount, 0);
}


// Test whether the rotation matrix and Euler angles are correct.
// Do this by checking consistency with vnl_rotation_matrix().
static void test_rotation_matrix_and_euler_angles()
{
  bool success = true;
  vnl_random rng(13241ul);
  const unsigned ntrials=100;
  for (unsigned i=0; i<ntrials; ++i)
  {
    bool this_trial_ok = true;
    double x = rng.drand32(-1.0, 1.0);
    double y = rng.drand32(-1.0, 1.0);
    double z = rng.drand32(-1.0, 1.0);
    vnl_vector_fixed<double,3> axis(x,y,z);
    axis.normalize();
    double ang = rng.drand32(-4*vnl_math::pi, 4*vnl_math::pi);

    // Construct the quaternion from this axis and angle,
    // and extract both euler_angles and rotation matrix.
    vnl_quaternion<double> q(axis, ang);
    vnl_vector_fixed<double,3> eu = q.rotation_euler_angles();
    vnl_matrix_fixed<double,3,3> R = (q.rotation_matrix_transpose()).transpose();

    // Use vnl_rotation_matrix() with axis+angle form
    {
      vnl_vector_fixed<double,3> axis_ang = axis * ang;
      vnl_matrix_fixed<double,3,3> M = vnl_rotation_matrix(axis_ang);
      vnl_matrix_fixed<double,3,3> D = R - M;
      double max_err = D.absolute_value_max();
      this_trial_ok = this_trial_ok && (max_err<=dtol);
#ifndef NDEBUG
      if (max_err>dtol)
      {
        vcl_cout << "Warning (a+a): max_err=" << max_err
                 << "  dtol=" << dtol << vcl_endl;
      }
#endif
    }

    // Use vnl_rotation_matrix() with euler angles.
    {
      vnl_vector<double> ex(3), ey(3), ez(3);
      ex[0]=1.0;  ex[1]=0.0;  ex[2]=0.0;
      ey[0]=0.0;  ey[1]=1.0;  ey[2]=0.0;
      ez[0]=0.0;  ez[1]=0.0;  ez[2]=1.0;
      ex *= eu[0];
      ey *= eu[1];
      ez *= eu[2];
      vnl_matrix<double> Rx = vnl_rotation_matrix(ex);
      vnl_matrix<double> Ry = vnl_rotation_matrix(ey);
      vnl_matrix<double> Rz = vnl_rotation_matrix(ez);
      vnl_matrix<double> M = Rz * Ry * Rx;
      vnl_matrix<double> D = R - M;
      double max_err = D.absolute_value_max();
      this_trial_ok = this_trial_ok && (max_err<=dtol);
#ifndef NDEBUG
      if (max_err>dtol)
      {
        vcl_cout << "Warning (ea): max_err=" << max_err
                 << "  dtol=" << dtol << vcl_endl;
      }
#endif
    }

    success = success && this_trial_ok;
  }
  TEST("test_rotation_matrix_and_euler_angles() for many trials", success, true);
}


static void test_rotations()
{
  vnl_vector_fixed<double,3> p1(2,2,2), p2(1,0,0), p3(0,1,0);
  vnl_vector_fixed<double,3> e0(0,0,0);
  vnl_quaternion<double> q0(0,0,0,0);
  TEST_NEAR("rotate p1 using q0", vnl_vector_ssd(q0.rotate(p1),p1), 0.0, 1e-8);
  TEST_NEAR("rotate p2 using q0", vnl_vector_ssd(q0.rotate(p2),p2), 0.0, 1e-8);
  vnl_quaternion<double> q0_b(0,0,0,1);
  TEST_NEAR("rotate p1 using q0_b", vnl_vector_ssd(q0_b.rotate(p1),p1), 0.0, 1e-8);
  TEST_NEAR("rotate p2 using q0_b", vnl_vector_ssd(q0_b.rotate(p2),p2), 0.0, 1e-8);
  TEST_NEAR("q0_b -> Euler angles", vnl_vector_ssd(q0_b.rotation_euler_angles(),e0), 0.0, 1e-8);
  vcl_cout << "q0_b -> Euler angles: " << q0_b.rotation_euler_angles() << vcl_endl;
  vnl_quaternion<double> q0_c(0,0,0,-4);
  TEST_NEAR("rotate p1 using q0_c", vnl_vector_ssd(q0_c.rotate(p1),p1), 0.0, 1e-8);
  TEST_NEAR("rotate p2 using q0_c", vnl_vector_ssd(q0_c.rotate(p2),p2), 0.0, 1e-8);
  vnl_quaternion<double> q0_d(0,0,0);
  TEST_NEAR("rotate p1 using q0_d", vnl_vector_ssd(q0_d.rotate(p1),p1), 0.0, 1e-8);
  TEST_NEAR("rotate p2 using q0_d", vnl_vector_ssd(q0_d.rotate(p2),p2), 0.0, 1e-8);

  // The axis replacing rotation - i.e. 120 degrees about (1,1,1)
  vnl_vector_fixed<double,3> e1(vnl_math::pi/2, 0, vnl_math::pi/2);
  vnl_quaternion<double> q1(p1/p1.magnitude(), vnl_math::pi * 2.0 / 3.0);
  TEST_NEAR("rotate p1 using q1", vnl_vector_ssd(q1.rotate(p1),p1), 0.0, 1e-8);
  TEST_NEAR("rotate p2 using q1", vnl_vector_ssd(q1.rotate(p2),p3), 0.0, 1e-8);
  vnl_vector_fixed<double,3> e1_b = q1.rotation_euler_angles();
  TEST_NEAR("q1 -> Euler angles", vnl_vector_ssd(e1_b,e1), 0.0, 1e-8);
  vnl_quaternion<double> q1_c = -q1;
  vnl_vector_fixed<double,3> e1_c = q1_c.rotation_euler_angles();
  TEST_NEAR("-q1 -> Euler angles", vnl_vector_ssd(e1_c,e1), 0.0, 1e-8);

  vcl_cout << "q1 -> Euler angles: " << e1 << vcl_endl;
  vnl_quaternion<double> q1_b(e1(0), e1(1), e1(2));
  vcl_cout << "q1 -> Euler angles: " << q1_b << vcl_endl;
  TEST_NEAR("Euler angles -> q1",
            vnl_vector_ssd(q1_b, q1), 0.0, 1e-8);

  vcl_cout << "Euler angles -> q1: " << q1_b << vcl_endl;

  test_random_round_trip();
  test_random_quat_near_zero();
  test_random_euler_near_zero();

  test_rotation_matrix_and_euler_angles();
}


// Main testing function
void test_quaternion()
{
  test_operators();
  test_rotations();
}


TESTMAIN(test_quaternion);
