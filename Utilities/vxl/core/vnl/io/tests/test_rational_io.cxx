// This is core/vnl/io/tests/test_rational_io.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_rational.h>
#include <vnl/io/vnl_io_rational.h>
#include <vsl/vsl_binary_io.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_rational_io()
{
  vcl_cout << "****************\n"
           << "test_rational_io\n"
           << "****************\n";

  vnl_rational nil(0L), inf(1L, 0L), one(2L,2L);

  vsl_b_ofstream bfs_out("vnl_rational_test_io.bvl.tmp");
  TEST ("Created vnl_rational_test_io.bvl.tmp for writing",
        (!bfs_out), false);
  vsl_b_write(bfs_out, nil);
  vsl_b_write(bfs_out, inf);
  vsl_b_write(bfs_out, one);
  bfs_out.close();

  vnl_rational r1, r2, r3;
  vsl_b_ifstream bfs_in("vnl_rational_test_io.bvl.tmp");
  TEST ("Opened vnl_rational_test_io.bvl.tmp for reading",
        (!bfs_in), false);
  vsl_b_read(bfs_in, r1);
  vsl_b_read(bfs_in, r2);
  vsl_b_read(bfs_in, r3);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_rational_test_io.bvl.tmp");

  TEST ("equality nil", nil, r1);
  TEST ("equality inf", inf, r2);
  TEST ("equality one", one, r3);

  vsl_print_summary(vcl_cout, nil);
  vsl_print_summary(vcl_cout, inf);
  vsl_print_summary(vcl_cout, one);
  vcl_cout << vcl_endl;
}

TESTMAIN(test_rational_io);
