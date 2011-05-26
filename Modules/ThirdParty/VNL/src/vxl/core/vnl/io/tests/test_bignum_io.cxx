// This is core/vnl/io/tests/test_bignum_io.cxx
#include <vcl_iostream.h>
#include <vnl/vnl_bignum.h>
#include <vnl/io/vnl_io_bignum.h>
#include <vsl/vsl_binary_io.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_bignum_io()
{
  vcl_cout << "**************\n"
           << "test_bignum_io\n"
           << "**************\n";

  vnl_bignum nil(0), big(-3245444l), verybig("4235702934875938745092384750293845"),
             p_inf("Infinity"), m_inf("-Inf");

  vsl_b_ofstream bfs_out("vnl_bignum_test_io.bvl.tmp");
  TEST ("Created vnl_bignum_test_io.bvl.tmp for writing",
        (!bfs_out), false);
  vsl_b_write(bfs_out, nil);
  vsl_b_write(bfs_out, big);
  vsl_b_write(bfs_out, verybig);
  vsl_b_write(bfs_out, p_inf);
  vsl_b_write(bfs_out, m_inf);
  bfs_out.close();

  vnl_bignum r1, r2, r3, r4, r5;
  vsl_b_ifstream bfs_in("vnl_bignum_test_io.bvl.tmp");
  TEST ("Opened vnl_bignum_test_io.bvl.tmp for reading",
        (!bfs_in), false);
  vsl_b_read(bfs_in, r1);
  vsl_b_read(bfs_in, r2);
  vsl_b_read(bfs_in, r3);
  vsl_b_read(bfs_in, r4);
  vsl_b_read(bfs_in, r5);
  TEST ("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_bignum_test_io.bvl.tmp");

  TEST ("equality 0", nil, r1);
  TEST ("equality -3245444", big, r2);
  TEST ("equality 4235702934875938745092384750293845", verybig, r3);
  TEST ("equality +Infinity", p_inf, r4);
  TEST ("equality -Infinity", m_inf, r5);

  vcl_cout << "\n0 summary: ";
  vsl_print_summary(vcl_cout, nil);
  vcl_cout << "\n-3245444 summary: ";
  vsl_print_summary(vcl_cout, r2);
  vcl_cout << "\n4235702934875938745092384750293845 summary: ";
  vsl_print_summary(vcl_cout, verybig);
  vcl_cout << "\n+Infinity summary: ";
  vsl_print_summary(vcl_cout, r4);
  vcl_cout << "\n-Infinity summary: ";
  vsl_print_summary(vcl_cout, m_inf);
  vcl_cout << vcl_endl;
}

TESTMAIN(test_bignum_io);
