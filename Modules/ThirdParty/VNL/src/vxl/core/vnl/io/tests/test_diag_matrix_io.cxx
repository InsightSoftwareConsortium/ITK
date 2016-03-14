// This is core/vnl/io/tests/test_diag_matrix_io.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/io/vnl_io_diag_matrix.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_diag_matrix_double_io()
{
  std::cout << "*******************\n"
           << "test_diag_matrix_io\n"
           << "*******************\n";
  //// test constructors, accessors
  const int n = 50;
  vnl_vector<double> v_out(n), v_in(n);

  for (int i=0; i<n; i++)
  {
    v_in(i) = (double)(i); // Different to check things change
    v_out(i) = (double)(i*i);
  }

  vnl_diag_matrix<double> diag_mat_out(v_out), diag_mat_in(v_in);

  vsl_print_summary(std::cout, diag_mat_out);
  std::cout << std::endl;

  vsl_b_ofstream bfs_out("vnl_diag_matrix_test_io.bvl.tmp");
  TEST ("Created vnl_diag_matrix_test_io.bvl.tmp for writing",
        (!bfs_out), false);
  vsl_b_write(bfs_out, diag_mat_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_diag_matrix_test_io.bvl.tmp");
  TEST ("Opened vnl_diag_matrix_test_io.bvl.tmp for reading",
        (!bfs_in), false);
  vsl_b_read(bfs_in, diag_mat_in);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_diag_matrix_test_io.bvl.tmp");

  TEST ("diag_mat_out.diagonal() == diag_mat_in.diagonal()",
        diag_mat_out.diagonal() == diag_mat_in.diagonal(), true);


  vsl_print_summary(std::cout, diag_mat_out);
  std::cout << std::endl;
}

void test_diag_matrix_io()
{
  test_diag_matrix_double_io();
}

TESTMAIN( test_diag_matrix_io );
