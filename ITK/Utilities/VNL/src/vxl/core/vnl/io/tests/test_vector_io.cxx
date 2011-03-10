// This is core/vnl/io/tests/test_vector_io.cxx
#include <vcl_iostream.h>
#include <vnl/io/vnl_io_vector.h>
#include <vsl/vsl_binary_io.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_vector_double_io()
{
  vcl_cout << "*****************************\n"
           << "Testing vnl_vector<double> io\n"
           << "*****************************\n";
  //// test constructors, accessors
  const int n = 50;
  vnl_vector<double> v_out(n), v_in;

  for (int i=0; i<n; i++)
  {
    v_out(i) = (double)(i*i);
  }

  vcl_cout << "before saving:\t"; vsl_print_summary(vcl_cout, v_out);

  vsl_b_ofstream bfs_out("vnl_vector_test_double_io.bvl.tmp");
  TEST ("Created vnl_vector_test_double_io.bvl.tmp for writing",
        (!bfs_out), false);
  vsl_b_write(bfs_out, v_out);
  bfs_out.close();

  vcl_cout << "after saving:\t"; vsl_print_summary(vcl_cout, v_out);

  vsl_b_ifstream bfs_in("vnl_vector_test_double_io.bvl.tmp");
  TEST ("Opened vnl_vector_test_double_io.bvl.tmp for reading",
        (!bfs_in), false);
  vsl_b_read(bfs_in, v_in);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vcl_cout << "after reading in:\t"; vsl_print_summary(vcl_cout, v_in);

  vpl_unlink ("vnl_vector_test_double_io.bvl.tmp");

  TEST ("v_out == v_in", v_out, v_in);

  vsl_print_summary(vcl_cout, v_out);
  vcl_cout << vcl_endl;
}


void test_vector_io()
{
  test_vector_double_io();
}


TESTMAIN(test_vector_io);
