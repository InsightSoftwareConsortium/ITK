#ifndef TESTLIB_REGISTER_H_
#define TESTLIB_REGISTER_H_
//:
// \file
// \author Amitha Perera
// \brief  Macros for registering the tests with the driver.
//
// A test driver program would simply look like
// \code
//   #include <testlib/testlib_register.h>
//   DECLARE( some_test_name );
//   void register_tests()
//   {
//     REGISTER( some_test_name );
//   }
//   DEFINE_MAIN;
// \endcode
// The DEFINE_MAIN macro will define the main() function for the driver.
// You will also have to link in a file defining a function
// \code
//   int some_test_name_main(int,char*[])
// \endcode
// See the vxl tests for further examples (such as vil/tests).

#include <string>
#include <vcl_compiler.h>

typedef int (*TestMainFunction)( int, char*[] );

//: Declare the existence of the test.
// If you DECLARE( x ), then you will need to define a function int x_main(int,char*[]).
#ifdef VCL_VC
#define DECLARE( testname ) int _cdecl testname ## _main ( int argc, char* argv[] )
#else
#define DECLARE( testname )  int testname ## _main ( int argc, char* argv[] )
#endif

void testlib_register_test(const std::string &, TestMainFunction);

//: Register the test with the driver.
// \param testname should be the same as one of the tests declared with DECLARE.
#define REGISTER( testname ) \
   testlib_register_test(#testname, & testname ## _main );

//: Define the main() routine for this test driver.
// This allows the main function to be defined in the driver code
// itself--instead of in the testlib library--thus avoiding
// "awf-weirdness". This also means that functionality from the test
// library, such as testlib_root_dir, can be used even if it is not
// used to create a test driver.
#define DEFINE_MAIN \
   int testlib_main(int,char*[]); \
   void testlib_cleanup(); \
   int main( int argc, char* argv[] ) { \
     register_tests(); \
     int retval = testlib_main( argc, argv ); \
     testlib_cleanup(); \
     return retval; \
   }

#endif // TESTLIB_REGISTER_H_
