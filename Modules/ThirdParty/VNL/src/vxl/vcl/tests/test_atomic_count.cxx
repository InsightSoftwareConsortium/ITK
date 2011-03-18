#include <vcl_atomic_count.h>
#include <vcl_cstdio.h>

#define TEST(str,x,y) vcl_printf(str ":   "); if (x!=y) { vcl_printf("FAILED\n"); status = 1; } else { vcl_printf("PASSED\n"); }

int test_atomic_count_main(int /*argc*/,char* /*argv*/[])
{
  int status = 0;

  vcl_atomic_count c(0);

  vcl_printf("\n");
  TEST("Initialization is correct", c, 0);

  ++c; // now c==1
  TEST("Increment is correct", c, 1);

  ++c; // now c==2
  TEST("Increment is correct", c, 2);

  for (int i=0; i<5;++i) ++c;
  // now c==7
  TEST("Increment in a loop is correct", c, 7);

  --c; // now c==6
  TEST("Decrement is correct", c, 6);

  --c; --c; // now c==4
  TEST("Decrement is correct", c, 4);

  ++c; // now c==5
  TEST("Increment is correct", c, 5);

  for (int i=0; i<4;++i) --c;
  // now c==1
  TEST("Decrement in a loop is correct", c, 1);

  // the last decrement
  TEST("Decrement to zero", --c, 0);

  //
  // Multiple instances do not affect each other
  //
  vcl_atomic_count d(1);

  ++c; ++d; ++d; --c; ++d;   // now c==0 and d==4
  TEST("Multiple instances act independently", (d==4&&c==0), true);
  return status;
}
