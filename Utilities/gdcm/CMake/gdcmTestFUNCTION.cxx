// Minimal test for existence of __FUNCTION__ pseudo-macro
#include <string.h>

int TestFUNCTION()
{
#ifdef __BORLANDC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ __FUNC__
  #endif
#endif
  const char *f = __FUNCTION__;
  int r = strcmp( "TestFUNCTION", f);
  return r;
}
 
int main()
{
  return TestFUNCTION();
}

