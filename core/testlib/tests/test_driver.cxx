#include <testlib/testlib_register.h>

DECLARE( test_assert );
DECLARE( test_macros );
DECLARE( test_args );
DECLARE( test_root_dir );

void
register_tests()
{
  REGISTER( test_assert );
  REGISTER( test_macros );
  REGISTER( test_args );
  REGISTER( test_root_dir );
}

DEFINE_MAIN;
