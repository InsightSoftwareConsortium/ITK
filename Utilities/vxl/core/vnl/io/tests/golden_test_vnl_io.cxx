// This is core/vnl/io/tests/golden_test_vnl_io.cxx

//:
// \file
// \brief Read in a golden data file, and check the values are correct.
//
// If you need to recreate the golden data file, run this test with
// the single parameter "create":
// \verbatim
//    golden_test_vnl_io create
// \endverbatim

#include <vcl_string.h>
#include <vcl_cstdlib.h> // for vcl_exit()
#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/io/vnl_io_vector.h>
#include <vnl/io/vnl_io_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/io/vnl_io_matrix.h>
#include <vnl/io/vnl_io_matrix_fixed.h>
#include <vnl/io/vnl_io_diag_matrix.h>
#include <vnl/io/vnl_io_real_npolynomial.h>
#include <vnl/io/vnl_io_real_polynomial.h>
#include <vnl/io/vnl_io_sparse_matrix.h>
#include <testlib/testlib_root_dir.h>
#include <testlib/testlib_test.h>

static void golden_test_vnl_io(bool save_file)
{
  vcl_cout << "***********************************************************\n"
           << " Testing a golden data file for cross platform consistency\n"
           << "***********************************************************\n";


  //-----------------------------------------------------------------------------------
  // Create objects:
  // If the "create" flag was used on the command line, then the program saves an example
  // of each class
  // Otherwise it just fills them with values for comparison to the values read in.
  //------------------------------------------------------------------------------------

  // vnl_vector
  const int n_vec = 50;
  vnl_vector<double> v_out(n_vec),v_in;

  for (int i=0; i<n_vec; i++)
  {
    v_out(i) = (double)(i*i);
  }

  // vnl_matrix
  const int m_mat = 10;
  const int n_mat = 6;
  vnl_matrix<double> m_out(m_mat, n_mat), m_in;

  for (int i=0; i<m_mat; i++)
  {
    for (int j=0; j<n_mat; j++)
    {
      m_out(i,j) = (double)(i*j+i);
    }
  }

  // vnl_diag_matrix
  vnl_diag_matrix<double> diag_mat_out(v_out), diag_mat_in;

  // vnl_matrix_fixed
  double datablock[4] = {
      1.1, 1.2,
      2.1, 2.2
    };
  vnl_matrix_fixed<double,2,2> m_fixed_out(datablock), m_fixed_in;

  // vnl_real_n_polynomial
  vnl_vector<double> coeffs(4);
  vnl_matrix<int> exponents(4,2);

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


  vnl_real_npolynomial polyn_out(coeffs, exponents), polyn_in;

  // vnl_real_polynomial
  vnl_real_polynomial poly_out(v_out), poly_in(0);

  // vnl_sparse_matrix
  vnl_sparse_matrix<double>  m_sparse_out(3,3), m_sparse_in(3,3);
  vcl_vector<int> col_1(3);
  vcl_vector<int> col_2(2);
  vcl_vector<int> col_3(1);

  col_1[0]=1;
  col_1[1]=2;
  col_1[2]=3;
  col_2[0]=1;
  col_2[1]=3;
  col_3[0]=2;

  vcl_vector<double> val_1(3);
  vcl_vector<double> val_2(2);
  vcl_vector<double> val_3(1);

  val_1[0]=1.1;
  val_1[1]=1.2;
  val_1[2]=1.3;
  val_2[0]=2.1;
  val_2[1]=2.3;
  val_3[0]=3.2;

  m_sparse_out.set_row(0, col_1, val_1);
  m_sparse_out.set_row(1, col_2, val_2);
  m_sparse_out.set_row(2, col_3, val_3);


  // vnl_vector_fixed
  vnl_vector_fixed<double,3> v_fixed_out(1.2,3.4,5.6), v_fixed_in;

  // Save if option set
  if (save_file)
  {
    vsl_b_ofstream bfs_out("golden_test_vnl_io.bvl");
    TEST ("Opened golden_test_vnl_io.bvl for writing ", ! bfs_out, false);
    if (!bfs_out)
    {
      vcl_cerr<<"Problems opening file for output - exiting\n";
      vcl_exit(1);
    }
    vsl_b_write(bfs_out, v_out);
    vsl_b_write(bfs_out, m_out);
    vsl_b_write(bfs_out, diag_mat_out);
    vsl_b_write(bfs_out, m_fixed_out);
    vsl_b_write(bfs_out, polyn_out);
    vsl_b_write(bfs_out, poly_out);
    vsl_b_write(bfs_out, m_sparse_out);
    vsl_b_write(bfs_out, v_fixed_out);
    bfs_out.close();
  }

  // Read in file to each class in turn
  vcl_string gold_path=testlib_root_dir()+"/core/vnl/io/tests/golden_test_vnl_io.bvl";
  vsl_b_ifstream bfs_in(gold_path.c_str());
  TEST ("Opened golden_test_vnl_io.bvl for reading ", ! bfs_in, false);
  vsl_b_read(bfs_in, v_in);
  vsl_b_read(bfs_in, m_in);
  vsl_b_read(bfs_in, diag_mat_in);
  vsl_b_read(bfs_in, m_fixed_in);
  vsl_b_read(bfs_in, polyn_in);
  vsl_b_read(bfs_in, poly_in);
  vsl_b_read(bfs_in, m_sparse_in);
  vsl_b_read(bfs_in, v_fixed_in);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();


  // Test that each object created is the same as read in from the file.
  TEST ("v_out == v_in", v_out == v_in, true);
  TEST ("m_out == m_in", m_out == m_in, true);
  TEST ("diag_mat_out == diag_mat_in", diag_mat_out.diagonal() == diag_mat_in.diagonal(), true);
  TEST ("m_fixed_out == m_fixed_in", m_fixed_out == m_fixed_in, true);
  TEST ("polyn_out == polyn_in", poly_out.coefficients() == poly_in.coefficients(), true);
  TEST ("poly_out == poly_in", poly_out.coefficients() == poly_in.coefficients(), true);


  //Code to compare sparse matrices
  m_sparse_out.reset();
  m_sparse_in.reset();
  bool test_result=true;

  while (m_sparse_out.next() && m_sparse_in.next())
  {
    if (m_sparse_out.getrow()!=m_sparse_in.getrow() || m_sparse_out.getcolumn() != m_sparse_in.getcolumn()
        ||  m_sparse_out.value()!= m_sparse_in.value())
    {
      test_result=false;
      break;
    }
  }

  TEST ("m_sparse_out == m_sparse_in",test_result , true);
  TEST ("v_fixed_out == v_fixed_in", v_fixed_out == v_fixed_in, true);
}


static void golden_test_vnl_io(int argc, char* argv[])
{
  golden_test_vnl_io(argc==2 && vcl_string(argv[1])==vcl_string("create"));
}

TESTMAIN_ARGS(golden_test_vnl_io);
