//:
// \file
// \brief vcl_tests.cxx : Run all vcl tests from one app.
//  I think this is preferable to having many vcl_test_* projects.
//  Note that all tests' main function should have signature (int,char**).
// \author awf, mar 2000

#include <vcl_iostream.h>
#include <vcl_string.h>

#if defined(VCL_BORLAND)
# include <math.h>
# include <float.h>
#endif // defined(VCL_BORLAND)

int test_algorithm_main(int, char*[]);
int test_cctype_main(int, char*[]);
int test_cmath_main(int, char*[]);
int test_compiler_main(int, char*[]);
int test_complex_main(int, char*[]);
int test_deque_main(int, char*[]);
int test_exception_main(int, char*[]);
int test_fstream_main(int, char*[]);
int test_headers_main(int, char**); // need not be called: just a compiler test
int test_iostream_main(int, char*[]);
int test_iterator_main(int, char*[]);
int test_list_main(int, char*[]);
int test_limits_main(int, char*[]);
int test_map_main(int, char*[]);
int test_multimap_main(int, char*[]);
int test_new_main(int, char*[]);
int test_set_main(int, char*[]);
int test_stlfwd_main(int, char*[]); // need not be called: just a compiler test
int test_string_main(int, char*[]);
int test_sstream_main(int, char*[]);
int test_vector_main(int, char*[]);
int test_cstdio_main(int, char*[]);

int passed;
int failed;

void testname( char* testname )
{
  vcl_cout << "   Testing vcl_" << testname << " ... ";
  vcl_cout.flush();
}

void testresult( int testresult )
{
  if ( testresult==0 ) {
    ++passed;
    vcl_cout << "    PASSED" << vcl_endl;
  } else {
    ++failed;
    vcl_cout << "  **FAILED**" << vcl_endl;
  }
}

// The else is for a trailing ; after the macro
#define DO_TEST( Name ) \
  if ( name == "" || name == "test_" #Name ) { \
    testname( #Name ); \
    testresult( test_##Name##_main(argc,argv) ); \
    test_run = 1; \
  } else

int main( int argc, char* argv[] )
{
  int test_run = 0;
  passed = failed = 0;
  vcl_string name = "";

  if ( argc > 1 ) {
    name = argv[1];
    ++argv;
    --argc;
  }

  // Disable Borland's floating point exceptions.
#if defined(VCL_BORLAND)
  _control87(MCW_EM, MCW_EM);  
#endif // defined(VCL_BORLAND)

  DO_TEST( algorithm );

  DO_TEST( cctype );

  DO_TEST( cmath );

  DO_TEST( compiler );

  DO_TEST( complex );

  DO_TEST( cstdio );

  DO_TEST( deque );

  DO_TEST( exception );

  DO_TEST( fstream );

  DO_TEST( iostream );

  DO_TEST( iterator );

  DO_TEST( list );

  DO_TEST( limits );

  DO_TEST( map );

  DO_TEST( multimap );

  DO_TEST( new );

  DO_TEST( set );

  DO_TEST( string );

  DO_TEST( sstream );

  DO_TEST( vector );

  if (test_run == 0)
  {
    vcl_cout << "Unsupported test " << name
             << "; should first be added to test_driver.cxx\n";
    failed = true;
  }

  vcl_cout << "Test Summary: ";
  if (failed > 0)
    vcl_cout<<passed<<" tests succeeded, "<<failed<<" tests failed\t\t\t*****";
  else if (passed > 1)
    vcl_cout<<"All "<<passed<<" tests succeeded";
  else
    vcl_cout<<"All tests succeeded";
  vcl_cout << "\n-----------------------------------------------------------------------------\n";

  return failed;
}
