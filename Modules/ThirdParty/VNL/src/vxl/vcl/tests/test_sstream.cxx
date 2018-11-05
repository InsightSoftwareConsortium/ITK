#include <string>
#include <iostream>
#include <sstream>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

#define AssertEq(x,y) {status+=((x)==(y))?0:1;std::cout<<"TEST ["<<(x)<<"] == ["<<(y)<<"] : "<<((x)==(y)?"PASSED":"FAILED")<<std::endl;}

int test_sstream_main(int /*argc*/,char* /*argv*/[])
{
  int status = 0;
  std::string x = "fred";
  std::istringstream ss(x);

  std::string fred;
  ss >> fred;
  AssertEq(fred,"fred");

  std::istringstream s("wilma");

  char w = '?';
  s >> w; AssertEq((int)w,'w');
  s >> w; AssertEq((int)w,'i');
  s >> w; AssertEq((int)w,'l');
  s >> w; AssertEq((int)w,'m');
  s >> w; AssertEq((int)w,'a');

  return status;
}
