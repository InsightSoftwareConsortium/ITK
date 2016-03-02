#include <testlib/testlib_test.h>

#include <vcl_string.h>

MAIN_ARGS( test_args )
{
  START( "argument passing" );
  TEST( "argument count (should be 3)", argc, 3 );
  if ( argc >= 1 )
  {
    testlib_test_begin( "argv[0] should be the test name, or \"all\"" );
    testlib_test_perform( vcl_string("test_args") == argv[0] ||
                          vcl_string("all") == argv[0]);
  }
  if ( argc >= 2 )
  {
    TEST( "argv[1] should be \"one\"", vcl_string("one"), argv[1] );
  }
  if ( argc >= 3 )
  {
    TEST( "argv[2] should be \"two\"", vcl_string("two"), argv[2] );
  }
  SUMMARY();
}
