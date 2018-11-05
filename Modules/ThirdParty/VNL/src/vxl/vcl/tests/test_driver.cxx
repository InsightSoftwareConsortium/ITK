//:
// \file
// \brief vcl_tests.cxx : Run all vcl tests from one app.
//  I think this is preferable to having many vcl_test_* projects.
//  Note that all tests' main function should have signature (int,char**).
// \author awf, mar 2000

#include <iostream>
#include <string>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

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
int test_memory_main(int, char*[]);
int test_multimap_main(int, char*[]);
int test_new_main(int, char*[]);
int test_set_main(int, char*[]);
int test_stlfwd_main(int, char*[]); // need not be called: just a compiler test
int test_string_main(int, char*[]);
int test_sstream_main(int, char*[]);
int test_vector_main(int, char*[]);
int test_cstdio_main(int, char*[]);
int test_preprocessor_main(int, char*[]);
int test_atomic_count_main(int, char*[]);
int test_typename_main(int, char*[]); // need not be called: just a compiler test

int passed;
int failed;

void testname( const char* testname )
{
  std::cout << "   Testing vcl_" << testname << " ... ";
  std::cout.flush();
}

void testresult( int testresult )
{
  if ( testresult==0 ) {
    ++passed;
    std::cout << "    PASSED" << std::endl;
  } else {
    ++failed;
    std::cout << "  **FAILED**" << std::endl;
  }
}

#define DO_TEST( Name ) \
  if ( name == "" || name == "test_" #Name ) { \
    testname( #Name ); \
    testresult( test_##Name##_main(argc,argv) ); \
    test_run = 1; \
  }

int main( int argc, char* argv[] )
{
  int test_run = 0;
  passed = failed = 0;
  std::string name = "";

  if ( argc > 1 ) {
    name = argv[1];
    ++argv;
    --argc;
  }

  DO_TEST(algorithm);
  DO_TEST(cctype);
  DO_TEST(cmath);
  DO_TEST(compiler);
  DO_TEST(complex);
  DO_TEST(cstdio);
  DO_TEST(deque);
  DO_TEST(exception);
  DO_TEST(fstream);
  DO_TEST(iostream);
  DO_TEST(iterator);
  DO_TEST(list);
  DO_TEST(limits);
  DO_TEST(memory);
  DO_TEST(map);
  DO_TEST(multimap);
  DO_TEST(set);
  DO_TEST(string);
  DO_TEST(sstream);
  DO_TEST(vector);
  DO_TEST(preprocessor);
  DO_TEST(atomic_count);

  if (test_run == 0)
  {
    std::cout << "Unsupported test " << name
             << "; should first be added to test_driver.cxx\n";
    failed = true;
  }

  std::cout << name << " Test Summary: ";
  if (failed > 0)
    std::cout<<passed<<" tests succeeded, "<<failed<<" tests failed\t\t\t*****";
  else if (passed > 1)
    std::cout<<"All "<<passed<<" tests succeeded";
  else
    std::cout<<"All tests succeeded";
  std::cout << "\n-----------------------------------------------------------------------------\n";

  return failed;
}
