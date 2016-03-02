// This is core/vnl/io/tests/test_matrix_io.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_matrix.h>
#include <vnl/io/vnl_io_matrix.h>
#include <vsl/vsl_binary_io.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_matrix_double_io()
{
  vcl_cout << "*****************************\n"
           << "Testing vnl_matrix<double> io\n"
           << "*****************************\n";
  //// test constructors, accessors
  const int m = 10;
  const int n = 6;
  vnl_matrix<double> m_out(m, n), m_in1(m,n),m_in2;

  for (int i=0; i<m; i++)
  {
    for (int j=0; j<n; j++)
    {
      m_out(i,j) = (double)(i*j+i);
      m_in1(i,j) = (double)(73);
    }
  }

  vsl_b_ofstream bfs_out("vnl_matrix_test_double_io.bvl.tmp");
  TEST("Created vnl_matrix_test_double_io.bvl.tmp for writing",
       (!bfs_out), false);
  vsl_b_write(bfs_out, m_out);
  vsl_b_write(bfs_out, m_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_matrix_test_double_io.bvl.tmp");
  TEST("Opened vnl_matrix_test_double_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, m_in1);
  vsl_b_read(bfs_in, m_in2);
  TEST("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_matrix_test_double_io.bvl.tmp");

  // m_in1 has content
  TEST("m_out == m_in1", m_out, m_in1);
  // m_in2 empty
  TEST("m_out == m_in2", m_out, m_in2);

  vsl_print_summary(vcl_cout, m_out);
  vcl_cout << vcl_endl;
}


void test_matrix_io()
{
  test_matrix_double_io();
}


TESTMAIN(test_matrix_io);
