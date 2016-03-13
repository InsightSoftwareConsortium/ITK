// This is core/vnl/io/tests/test_sparse_matrix_io.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/io/vnl_io_sparse_matrix.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

static bool Compare(vnl_sparse_matrix<double>& M1, vnl_sparse_matrix<double>& M2)
{
  M1.reset();
  M2.reset();

  while (M1.next() && M2.next())
  {
    if (M1.getrow()!=M2.getrow() || M1.getcolumn() != M2.getcolumn() || M1.value()!= M2.value())
    {
      return false;
    }
  }

  return true;
}

void test_sparse_matrix_double_io()
{
  std::cout << "************************************\n"
           << "Testing vnl_sparse_matrix<double> io\n"
           << "************************************\n";
  //// test constructors, accessors

  vnl_sparse_matrix<double>  m_out(3,3), m_in0(3,3), m_in1(3,3);
  vnl_sparse_matrix<double>  m10000_out(10000,10000), m10000_in(10000,10000);
  std::vector<int> col_1(3);
  std::vector<int> col_2(2);
  std::vector<int> col_3(1);

  col_1[0]=1;
  col_1[1]=2;
  col_1[2]=3;
  col_2[0]=1;
  col_2[1]=3;
  col_3[0]=2;

  std::vector<double> val_1(3);
  std::vector<double> val_2(2);
  std::vector<double> val_3(1);

  val_1[0]=1.1;
  val_1[1]=1.2;
  val_1[2]=1.3;
  val_2[0]=2.1;
  val_2[1]=2.3;
  val_3[0]=3.2;

  m_out.set_row(0, col_1, val_1);
  m_out.set_row(1, col_2, val_2);
  m_out.set_row(2, col_3, val_3);

  vsl_b_ofstream bfs_out("vnl_sparse_matrix_io.bvl.tmp");
  TEST ("vnl_sparse_matrix_io.bvl.tmp for writing", (!bfs_out), false);
  vsl_b_write(bfs_out, m_out);
  vsl_b_write(bfs_out, m_out);
  vsl_b_write(bfs_out, m10000_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_sparse_matrix_io.bvl.tmp");
  TEST ("vnl_sparse_matrix_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, m_in0);
  vsl_b_read(bfs_in, m_in1);
  vsl_b_read(bfs_in, m10000_in);

  bfs_in.close();

  vpl_unlink ("vnl_sparse_matrix_io.bvl.tmp");

  TEST ("m_out == m_in0",Compare(m_out,m_in0) , true);
  TEST ("m_out == m_in1",Compare(m_out,m_in1) , true);
  TEST ("m10000_out == m10000_in",Compare(m10000_out,m10000_in) , true);

  vsl_print_summary(std::cout, m_out);
  std::cout << std::endl;
}


void test_sparse_matrix_io()
{
  test_sparse_matrix_double_io();
}


TESTMAIN(test_sparse_matrix_io);
