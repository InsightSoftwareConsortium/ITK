// This is core/vnl/io/tests/test_real_npolynomial_io.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_real_npolynomial.h>
#include <vnl/io/vnl_io_real_npolynomial.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_real_npolynomial_io()
{
  std::cout << "************************\n"
           << "test_real_npolynomial_io\n"
           << "************************\n";
  //// test constructors, accessors
  vnl_vector<double> coeffs(4),coeffs2;
  vnl_matrix<unsigned int> exponents(4,2);

  coeffs(0) = 0.1;
  coeffs(1) = 0.2;
  coeffs(2) = 0.3;
  coeffs(3) = 0.5;

  exponents(0,0) = 1;
  exponents(1,0) = 2;
  exponents(2,0) = 3;
  exponents(3,0) = 4;
  exponents(0,1) = 5;
  exponents(1,1) = 6;
  exponents(2,1) = 7;
  exponents(3,1) = 8;

  coeffs2 = coeffs*2.0;

  //vsl_print_summary(std::cout, coeffs);

  vnl_real_npolynomial poly_out(coeffs, exponents), poly_in0,poly_in1(coeffs2,exponents);

  vsl_b_ofstream bfs_out("vnl_real_npolynomial_test_io.bvl.tmp");
  TEST ("Created vnl_real_npolynomial_test_io.bvl.tmp for writing",
        (!bfs_out), false);
  vsl_b_write(bfs_out, poly_out);
  vsl_b_write(bfs_out, poly_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_real_npolynomial_test_io.bvl.tmp");
  TEST ("Opened vnl_real_npolynomial_test_io.bvl.tmp for reading",
        (!bfs_in), false);
  vsl_b_read(bfs_in, poly_in0);
  vsl_b_read(bfs_in, poly_in1);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_real_npolynomial_test_io.bvl.tmp");

  TEST ("poly_out.coefficients() == poly_in0.coefficients()",
        poly_out.coefficients() == poly_in0.coefficients(), true);
  TEST ("poly_out.coefficients() == poly_in1.coefficients()",
        poly_out.coefficients() == poly_in1.coefficients(), true);

  vsl_print_summary(std::cout, poly_out);
  std::cout << std::endl;
}

TESTMAIN(test_real_npolynomial_io);
