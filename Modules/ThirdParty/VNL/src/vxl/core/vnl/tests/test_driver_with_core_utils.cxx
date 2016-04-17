#include <testlib/testlib_register.h>

DECLARE( test_matlab );
DECLARE( test_sample );

void
register_tests()
{
  REGISTER( test_matlab );
  REGISTER( test_sample );
}

DEFINE_MAIN;
