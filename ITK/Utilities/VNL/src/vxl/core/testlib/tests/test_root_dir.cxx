#include <vcl_iostream.h>
#include <vcl_fstream.h>
#include <testlib/testlib_test.h>
#include <testlib/testlib_root_dir.h>

static void test_root_dir()
{
  // Check that a file exists
  vcl_string path = testlib_root_dir() + "/core/testlib/testlib_root_dir.h";

  vcl_fstream is(path.c_str(),vcl_ios_in);

  TEST ("Opening file using testlib_root_dir", !is, false);

  if (!is)
  {
    vcl_cerr<<"Unable to open "<<path<<"\ntestlib_root_dir() is probably wrong.\n"
              "Try setting $VXLSRC to the source root directory.\n";
  }
  else
  {
    is.close();
    vcl_cout<<"Root Dir: "<<testlib_root_dir()<<" appears to be correct.\n";
  }
}

TESTMAIN(test_root_dir);
