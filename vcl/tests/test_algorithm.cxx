/*
  fsm
*/
#include <vcl_algorithm.h>

int test_algorithm_main(int /*argc*/,char* /*argv*/[]) 
{
  double v[5] = {1,5,2,4,3};
  vcl_sort(v, v+5);

  return 0;
}
