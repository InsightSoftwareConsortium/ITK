// This is core/vnl/io/tests/test_matrix_fixed_io.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/io/vnl_io_matrix_fixed.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_matrix_fixed_double_2_2_io()
{
  vcl_cout << "***************************************\n"
           << "Testing vnl_matrix_fixed<double,2,2> io\n"
           << "***************************************\n";
  //// test constructors, accessors
   double datablock[4] = {
                           1.1, 1.2,
                           2.1, 2.2
                         };
  vnl_matrix_fixed<double,2,2> m_out(datablock), m_in0,m_in1;

  // Give some initial content
  m_in1 = m_out * 2.0;

  vsl_b_ofstream bfs_out("vnl_matrix_fixed_io.bvl.tmp",
                         vcl_ios_out | vcl_ios_binary);
  TEST ("vnl_matrix_fixed_io.bvl.tmp for writing", (!bfs_out), false);
  vsl_b_write(bfs_out, m_out);
  vsl_b_write(bfs_out, m_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_matrix_fixed_io.bvl.tmp",
                        vcl_ios_in | vcl_ios_binary);
  TEST ("vnl_matrix_fixed_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, m_in0);
  vsl_b_read(bfs_in, m_in1);
  bfs_in.close();

  vpl_unlink ("vnl_matrix_fixed_io.bvl.tmp");

  // m_in0 is initially empty
  TEST ("m_out == m_in0", m_out, m_in0);
  // m_in1 has content
  TEST ("m_out == m_in1", m_out, m_in1);

  vsl_print_summary(vcl_cout, m_out);
  vcl_cout << vcl_endl;
}


void test_matrix_fixed_io()
{
  test_matrix_fixed_double_2_2_io();
}


TESTMAIN(test_matrix_fixed_io);
