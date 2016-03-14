#include <iostream>
#include <fstream>
#include <vcl_compiler.h>
#include <testlib/testlib_test.h>
#include <testlib/testlib_root_dir.h>

static void test_root_dir()
{
  // Check that a file exists
  std::string path = testlib_root_dir() + "/core/testlib/testlib_root_dir.h";

  std::fstream is(path.c_str(),std::ios::in);

  TEST ("Opening file using testlib_root_dir", !is, false);

  if (!is)
  {
    std::cerr<<"Unable to open "<<path<<"\ntestlib_root_dir() is probably wrong.\n"
              "Try setting $VXLSRC to the source root directory.\n";
  }
  else
  {
    is.close();
    std::cout<<"Root Dir: "<<testlib_root_dir()<<" appears to be correct.\n";
  }
}

TESTMAIN(test_root_dir);
