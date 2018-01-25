// This is vcl/tests/test_exception.cxx
#include <vcl_exception.h>
#include <vcl_iostream.h>

int test_exception_main(int /*argc*/,char* /*argv*/[])
{
#if VCL_HAS_EXCEPTIONS
  const char *ex = "\"const char* exception\"";
  int result;
  vcl_try {
    vcl_cout << "throw " << ex << vcl_endl;
    vcl_throw ex;
  }
  vcl_catch (const char* e) {
    vcl_cout << "caught " << e << ".  Good." << vcl_endl;
    result = 0;
  }
  vcl_catch_all {
    vcl_cout << "caught nothing.  Bad." << vcl_endl;
    result = 1;
  }
  return result;
#else
  vcl_cout << "this compiler does not support exception handling\n";
  return 0;
#endif
}
