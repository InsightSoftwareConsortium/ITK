#include <stdio.h>

#ifdef _WIN32
typedef unsigned __int64 nio_ullong;
#else
typedef unsigned long long nio_ullong;
#endif

typedef union {
  double d;
  struct {
    nio_ullong bogus0 : 60;
    unsigned int bogus1 : 2;
    unsigned int bogus2 : 2;
  } c;
} nio_test;

int
main(int argc, char *argv[]) {
  
  printf("hello, world.\n");
  return 0;
}
