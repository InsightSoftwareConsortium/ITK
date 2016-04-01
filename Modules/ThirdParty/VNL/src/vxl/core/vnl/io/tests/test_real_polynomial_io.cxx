// This is core/vnl/io/tests/test_real_polynomial_io.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/vnl_real_polynomial.h>
#include <vnl/io/vnl_io_real_polynomial.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_real_polynomial_io()
{
  std::cout << "******************************\n"
           << "Testing vnl_real_polynomial io\n"
           << "******************************\n";
  //// test constructors, accessors
  const int n = 10;
  vnl_vector<double> v(n);

  for (int i=0; i<n; i++)
  {
      v(i) = (double)(i*i);
  }

  vnl_real_polynomial poly_out(v), poly_in0(0),poly_in1(v*2.0);


  vsl_b_ofstream bfs_out("vnl_real_polynomial_test_io.bvl.tmp");
  TEST("Created vnl_real_polynomial_test_io.bvl.tmp for writing", (!bfs_out), false);
  vsl_b_write(bfs_out, poly_out);
  vsl_b_write(bfs_out, poly_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_real_polynomial_test_io.bvl.tmp");
  TEST("Opened vnl_real_polynomial_test_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, poly_in0);
  vsl_b_read(bfs_in, poly_in1);
  TEST("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_real_polynomial_test_io.bvl.tmp");

  TEST("poly_out.coefficients() == poly_in0.coefficients()",
       poly_out.coefficients() == poly_in0.coefficients(), true);
  TEST("poly_out.coefficients() == poly_in1.coefficients()",
       poly_out.coefficients() == poly_in1.coefficients(), true);

  vsl_print_summary(std::cout, poly_in0);
  std::cout << std::endl;
}


TESTMAIN(test_real_polynomial_io);
