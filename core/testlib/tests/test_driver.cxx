#include "testlib/testlib_register.h"

DECLARE(test_assert);
DECLARE(test_macros);
DECLARE(test_args);

void
register_tests()
{
  REGISTER(test_assert);
  REGISTER(test_macros);
  REGISTER(test_args);
}

DEFINE_MAIN;
