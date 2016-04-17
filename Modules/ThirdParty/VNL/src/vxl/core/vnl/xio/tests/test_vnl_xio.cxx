#include <sstream>
#include <vnl/xio/vnl_xio_matrix_fixed.h>
#include <vnl/xio/vnl_xio_matrix.h>
#include <vnl/xio/vnl_xio_vector_fixed.h>
#include <vnl/xio/vnl_xio_vector.h>
#include <vnl/xio/vnl_xio_quaternion.h>

#include <testlib/testlib_test.h>
#include <vcl_compiler.h>

static void test_xtreeio_matrix_fixed()
{
  std::stringstream s;

  double data_m[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  vnl_matrix_fixed<double,2,4> m(data_m);
  x_write_tree(s, m);
  TEST("XML I/O for vnl_matrix_fixed<double,2,4>", s.str(),
       "<vnl_matrix_fixed rows=\"2\" cols=\"4\">\n<row> <cell> 1.000000 </cell> <cell> 2.000000 </cell> <cell> 3.000000 </cell> <cell> 4.000000 </cell> </row> <row> <cell> 5.000000 </cell> <cell> 6.000000 </cell> <cell> 7.000000 </cell> <cell> 8.000000 </cell> </row>\n</vnl_matrix_fixed>\n");
}

static void test_xtreeio_matrix()
{
  std::stringstream s;

  double data_m[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  vnl_matrix<double> m(data_m,2,4);
  x_write_tree(s, m);
  TEST("XML I/O for vnl_matrix<double>", s.str(),
       "<vnl_matrix rows=\"2\" cols=\"4\">\n<row> <cell> 1.000000 </cell> <cell> 2.000000 </cell> <cell> 3.000000 </cell> <cell> 4.000000 </cell> </row> <row> <cell> 5.000000 </cell> <cell> 6.000000 </cell> <cell> 7.000000 </cell> <cell> 8.000000 </cell> </row>\n</vnl_matrix>\n");
}

static void test_xtreeio_vector_fixed()
{
  std::stringstream s;

  vnl_vector_fixed<double,3> vf(10.0,20.0,5.0);
  x_write_tree(s, vf);
  TEST("XML I/O for vnl_vector_fixed<double,3>", s.str(),
       "<vnl_vector_fixed size=\"3\">\n<element> 10.000000 </element> <element> 20.000000 </element> <element> 5.000000 </element>\n</vnl_vector_fixed>\n");
}

static void test_xtreeio_vector()
{
  std::stringstream s;

  double data_v[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  vnl_vector<double> v(data_v,9);
  x_write_tree(s, v);
  TEST("XML I/O for vnl_vector<double>", s.str(),
       "<vnl_vector size=\"9\">\n<element> 1.000000 </element> <element> 2.000000 </element> <element> 3.000000 </element> <element> 4.000000 </element> <element> 5.000000 </element> <element> 6.000000 </element> <element> 7.000000 </element> <element> 8.000000 </element> <element> 9.000000 </element>\n</vnl_vector>\n");
}

static void test_xtreeio_quaternion()
{
  std::stringstream s;

  vnl_quaternion<double> q(1,2,3,4);
  x_write_tree(s, q);
  TEST("XML I/O for vnl_quaternion<double>", s.str(),
       "<vnl_quaternion>\n<x> 1.000000 </x> <y> 2.000000 </y> <z> 3.000000 </z> <r> 4.000000 </r>\n</vnl_quaternion>\n");
}

static void test_xio_matrix_fixed()
{
  std::stringstream s;

  double data_m[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  vnl_matrix_fixed<double,2,4> m(data_m);
  x_write(s, m);
  TEST("XML I/O for vnl_matrix_fixed<double,2,4>", s.str(),
       "<vnl_matrix_fixed rows=\"2\" cols=\"4\">\n1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 8.000000\n</vnl_matrix_fixed>\n");
}

static void test_xio_matrix()
{
  std::stringstream s;

  double data_m[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  vnl_matrix<double> m(data_m,2,4);
  x_write(s, m);
  TEST("XML I/O for vnl_matrix<double>", s.str(),
       "<vnl_matrix rows=\"2\" cols=\"4\">\n1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 8.000000\n</vnl_matrix>\n");
}

static void test_xio_vector_fixed()
{
  std::stringstream s;

  vnl_vector_fixed<double,3> vf(10.0,20.0,5.0);
  x_write(s, vf);
  TEST("XML I/O for vnl_vector_fixed<double,3>", s.str(),
       "<vnl_vector_fixed size=\"3\">\n10.000000 20.000000 5.000000\n</vnl_vector_fixed>\n");
}

static void test_xio_vector()
{
  std::stringstream s;

  double data_v[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  vnl_vector<double> v(data_v,9);
  x_write(s, v);
  TEST("XML I/O for vnl_vector<double>", s.str(),
       "<vnl_vector size=\"9\">\n1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 8.000000 9.000000\n</vnl_vector>\n");
}

static void test_xio_quaternion()
{
  std::stringstream s;

  vnl_quaternion<double> q(1,2,3,4);
  x_write(s, q);
  TEST("XML I/O for vnl_quaternion<double>", s.str(),
       "<vnl_quaternion x=\"1.000000\" y=\"2.000000\" z=\"3.000000\" r=\"4.000000\">\n</vnl_quaternion>\n");
}

static void test_vnl_xio()
{
  test_xtreeio_matrix_fixed();
  test_xtreeio_matrix();
  test_xtreeio_vector_fixed();
  test_xtreeio_vector();
  test_xtreeio_quaternion();
  test_xio_matrix_fixed();
  test_xio_matrix();
  test_xio_vector_fixed();
  test_xio_vector();
  test_xio_quaternion();
}

TESTMAIN(test_vnl_xio);
