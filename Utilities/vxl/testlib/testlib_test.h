// This is core/testlib/testlib_test.h
#ifndef testlib_test_h_
#define testlib_test_h_
//:
// \file
// \brief Testing software
// \author Tim Cootes
// \verbatim
// Modifications:
//   Apr 2002, Amitha Perera: Copied from vil_test and moved into testlib in an
//     attempt to consolidate all the test repeated test functionality.
// \endverbatim

#include <vcl_string.h>
#include <vcl_complex_fwd.h>

//: initialise test counters, check test name 'name' exists
void testlib_test_start(const char* name = 0);
//: increment number of tests, then output msg
void testlib_test_begin(const char* msg);
//: increment success/failure counters
void testlib_test_perform(bool success);
//: output summary of tests performed
int  testlib_test_summary();

//: output msg, then perform test in expr
void testlib_test_assert(const vcl_string& msg, bool expr);
//: output msg, then perform test to see if expr is within tol of target
void testlib_test_assert_near(const vcl_string& msg, double expr,
                              double target = 0, double tol = 1e-12);
//: output msg, then perform test to see if expr is within tol of target
void testlib_test_assert_near(const vcl_string& msg, vcl_complex<double> expr,
                              vcl_complex<double> target, double tol = 1e-12);
//: output msg, then perform test to see if expr is not within tol of target
void testlib_test_assert_far(const vcl_string& msg, double expr,
                             double target = 0, double tol = 1e-12);
//: output msg, then perform test to see if expr is not within tol of target
void testlib_test_assert_far(const vcl_string& msg, vcl_complex<double> expr,
                             vcl_complex<double> target, double tol = 1e-12);

#define Assert testlib_test_assert
#define AssertNear testlib_test_assert_near
#define AssertFar testlib_test_assert_far

//: initialise test
#define START(s) testlib_test_start(s);

//: TEST function, s is message, test to see if p==v
#define TEST(s,p,v) \
do { \
  testlib_test_begin(s); \
  testlib_test_perform((p)==(v)); \
} while (0)

//: TEST function, s is message, test to see if p is close to v, tolerence t
#define TEST_NEAR(s,p,v,t) \
do { \
  testlib_test_begin(s); \
  testlib_test_assert_near("",p,v,t); \
} while (0)

//: TEST function, s is message, test to see if p is far from v, tolerence t
#define TEST_FAR(s,p,v,t) \
do { \
  testlib_test_begin(s); \
  testlib_test_assert_far("",p,v,t); \
} while (0)

//: run x, s is message, then test to see if p==v
#define TEST_RUN(s,x,p,v) \
do { \
  testlib_test_begin(s); \
  x; \
  testlib_test_perform((p)==(v)); \
} while (0)

//: summarise test
#define SUMMARY() return testlib_test_summary();

//: run a singleton test function
#define RUN_TEST_FUNC(x) \
  testlib_test_start(#x); x(); return testlib_test_summary();

//: declare the main function
#define MAIN( testname ) \
  int testname ## _main( int argc, char* argv[] )
 
//: a simplified version of the main test, just in one line
#define TESTMAIN( testname ) \
  MAIN( testname ) { START( #testname ); testname(); SUMMARY(); }

//: Another simplified main test.
#undef TESTLIB_DEFINE_MAIN
#define TESTLIB_DEFINE_MAIN(x) \
int main() { testlib_test_start(#x); x(); return testlib_test_summary(); }

//: A simplified main test with parameter passing.
#undef TESTLIB_DEFINE_MAIN_ARGS
#define TESTLIB_DEFINE_MAIN_ARGS(x) \
int main(int argc, char * argv[]) { testlib_test_start(#x); x(argc,argv); return testlib_test_summary(); }

#endif // testlib_test_h_
