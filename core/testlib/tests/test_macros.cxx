#include <testlib/testlib_test.h>

static int val = 0;

void
my_function()
{
  val = 1;
}

int
test_macros_main( int, char*[] )
{
  START( "macros" );
  TEST( "TEST macro", 5, 5 );
  TEST_RUN( "TEST_RUN macro", my_function(), val, 1 );
  SUMMARY();
}
