#include <testlib/testlib_test.h>

static void test_assert()
{
  testlib_test_assert( "Assert: ", true );
  testlib_test_assert_near( "Assert near: ", 1.2345, 1.2346, 0.001 );
}

TESTMAIN(test_assert);
