#include <vcl_iostream.h>
#include <vcl_iomanip.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_quaternion.h>


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
      rng.normal()*vnl_math::pi/18.0, rng.normal()*vnl_math::pi/18.0);
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
      rng.normal()*vnl_math::pi/180.0, rng.normal()*vnl_math::pi/180.0);
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

}

void test_quaternion()
{
  test_operators();
  test_rotations();
}

TESTMAIN(test_quaternion);
