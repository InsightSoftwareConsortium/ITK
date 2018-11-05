#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>
#include <exception>
#include "testlib_register.h"

#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

#if defined(_MSC_VER)
#  include <crtdbg.h>
#  include <windows.h>
#include <cstdio>

LONG WINAPI vxl_exception_filter( struct _EXCEPTION_POINTERS *ExceptionInfo )
{
  // Retrieve exception information
  PVOID ExceptionAddress       = ExceptionInfo->ExceptionRecord->ExceptionAddress;
  DWORD ExceptionCode          = ExceptionInfo->ExceptionRecord->ExceptionCode;
  DWORD* ExceptionInformation  = (DWORD*)ExceptionInfo->ExceptionRecord->ExceptionInformation;

  std::fprintf(stderr, "\nTOP-LEVEL EXCEPTION HANDLER\n");
  switch (ExceptionCode)
  {
   case EXCEPTION_ACCESS_VIOLATION:
    std::fprintf(stderr, "The instruction at \"0x%.8p\" failed to %s memory at \"0x%.8x\".\n\n",
                ExceptionAddress, ExceptionInformation[0] ? "write to" :"read",
                ExceptionInformation[1]);
    break;

   case EXCEPTION_INT_DIVIDE_BY_ZERO:
    std::fprintf(stderr, "The instruction at \"0x%.8p\" caused an exception of integer devision by zero.\n\n",
                ExceptionAddress);
    break;
   default:
    std::fprintf(stderr, "The instruction at \"0x%.8p\" caused an unknown exception (exception code: \"0x%.8x\").\n\n",
                ExceptionAddress,
                ExceptionCode);
  }

  // Default action is to abort
  std::printf("Execution aborted!\n");
  return EXCEPTION_EXECUTE_HANDLER;
}
#endif // defined(_WIN32)

static std::vector<TestMainFunction> testlib_test_func_;
static std::vector<std::string>       testlib_test_name_;

void
list_test_names( std::ostream& ostr )
{
  ostr << "The registered test names are:\n";
  for (const auto & i : testlib_test_name_)
    ostr << "   " << i << '\n';
  ostr << "\nOmitting a test name, or specifying the name \"all\" will run all the tests.\n";
}


void
testlib_enter_stealth_mode()
{
  // check for Dashboard test
  char * env_var1 = std::getenv("DART_TEST_FROM_DART");
  char * env_var2 = std::getenv("DASHBOARD_TEST_FROM_CTEST");  // DART Client built in CMake
  if ( env_var1 || env_var2 ) {

  // Don't allow DART test to open critical error dialog boxes
#if defined(_MSC_VER)
    // No abort or ANSI assertion failure dialog box
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);

    // No Windows style ASSERT failure dialog box
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);

    // No unhandled exceptions dialog box,
    // such as access violation and integer division by zero
    SetUnhandledExceptionFilter( vxl_exception_filter );
#endif //defined(_MSC_VER)

    // Disable Borland's floating point exceptions.
  }
}

int testlib_run_test_unit(std::vector<std::string>::size_type i, int argc, char *argv[])
{
  char * env_var1 = std::getenv("DART_TEST_FROM_DART");
  char * env_var2 = std::getenv("DASHBOARD_TEST_FROM_CTEST");  // DART Client built in CMake
  if ( env_var1 || env_var2 ) {
    try {
      return testlib_test_func_[i]( argc, argv );
    }
    catch (const std::exception &e)
    {
      std::cerr << "\nTOP-LEVEL EXCEPTION HANDLER                                        **FAILED**\n"
               << e.what() << "\n\n";
      return 1;
    }
  }
  return testlib_test_func_[i]( argc, argv );
}


int
testlib_main( int argc, char* argv[] )
{
  // The caller should already have called register_tests().

  // NOT to produce any dialog windows
  testlib_enter_stealth_mode();

  // Assume the index type for vector<string> and
  // vector<TestMainFunction> are the same.
  typedef std::vector<std::string>::size_type vec_size_t;

  // Error check.
  if ( testlib_test_func_.size() != testlib_test_name_.size() ) {
    std::cerr << "Error: " << testlib_test_func_.size() << " test functions are registered, but "
             << testlib_test_name_.size() << " test names are registered.\n";
    return 1;
  }


  // If a test name is given, try to run it. Otherwise, try to run all
  // the tests. The first argument, if available, is assumed to be a
  // test name. The special test name "all" can be used to run all the tests
  // with the subsequent arguments passed to each test.

  bool test_name_given = argc >= 2;

  if ( test_name_given && std::string("all") == argv[1] )
  {
    --argc; ++argv; test_name_given = false;
  }
  if ( test_name_given )
  {
    for ( vec_size_t i = 0; i < testlib_test_name_.size(); ++i )
      if ( testlib_test_name_[i] == argv[1] )
        return testlib_run_test_unit(i, argc-1, argv+1);


    std::cerr << "Test " << argv[1] << " not registered.\n";
    list_test_names( std::cerr );
  }
  else
  {
    std::cout << "No test name provided.  Attempting to run all tests.\n";
    list_test_names( std::cout );
    std::cout << "If you want to run a single test, specify one of the above on the command line.\n\n" << std::flush;

    bool all_pass = true;
    for ( vec_size_t i = 0; i < testlib_test_name_.size(); ++i )
    {
      std::cout << "----------------------------------------\n"
               << "Running: " << testlib_test_name_[i] << '\n'
               << "----------------------------------------\n" << std::flush;

      int result = testlib_run_test_unit(i, argc, argv);

      std::cout << "----------------------------------------\n"
               << testlib_test_name_[i] << " returned " << result << ' '
               << ( result==0 ? "(PASS)" : "(FAIL)" ) << '\n'
               << "----------------------------------------\n" << std::flush;
      all_pass &= (result == 0);
    }

    std::cout << "\n\nCombined result of " << testlib_test_name_.size() << " tests: "
             << ( all_pass ? "PASS" : "FAIL" ) << std::endl;
    return all_pass ? 0 : 1;
  }

  return 1;
}

void testlib_register_test(const std::string & name, TestMainFunction func)
{
  testlib_test_func_.push_back(func);
  testlib_test_name_.push_back(name);
}


void testlib_cleanup()
{
  testlib_test_func_.clear();
  testlib_test_func_.clear();
}
