// This is core/vnl/io/tests/test_vector_fixed_io.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/io/vnl_io_vector_fixed.h>
#include <vsl/vsl_binary_io.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_vector_fixed_double_3_io()
{
  vcl_cout << "*************************************\n"
           << "Testing vnl_vector_fixed<double,3> io\n"
           << "*************************************\n";
  //// test constructors, accessors

  vnl_vector_fixed<double,3> m_out(1.2,3.4,5.6), m_in;

  vsl_b_ofstream bfs_out("vnl_vector_fixed_io.bvl.tmp");
  TEST ("vnl_vector_fixed_io.bvl.tmp for writing", (!bfs_out), false);
  vsl_b_write(bfs_out, m_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_vector_fixed_io.bvl.tmp");
  TEST ("vnl_vector_fixed_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, m_in);
  bfs_in.close();

  vpl_unlink ("vnl_vector_fixed_io.bvl.tmp");

  TEST ("m_out == m_in", m_out, m_in);

  vsl_print_summary(vcl_cout, m_out);
  vcl_cout << vcl_endl;
}


void test_vector_fixed_io()
{
  test_vector_fixed_double_3_io();
}


TESTMAIN(test_vector_fixed_io);
