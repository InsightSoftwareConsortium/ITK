#include <string>
#include <testlib/testlib_test.h>

#include <vcl_compiler.h>

MAIN_ARGS( test_args )
{
  START( "argument passing" );
  TEST( "argument count (should be 3)", argc, 3 );
  if ( argc >= 1 )
  {
    testlib_test_begin( "argv[0] should be the test name, or \"all\"" );
    testlib_test_perform( std::string("test_args") == argv[0] ||
                          std::string("all") == argv[0]);
  }
  if ( argc >= 2 )
  {
    TEST( "argv[1] should be \"one\"", std::string("one"), argv[1] );
  }
  if ( argc >= 3 )
  {
    TEST( "argv[2] should be \"two\"", std::string("two"), argv[2] );
  }
  SUMMARY();
}
