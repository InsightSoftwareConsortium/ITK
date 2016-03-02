#include <vcl_string.h> // C++ specific includes first
#include <vcl_iostream.h>
#include <vcl_sstream.h>

#define AssertEq(x,y) {status+=((x)==(y))?0:1;vcl_cout<<"TEST ["<<x<<"] == ["<<y<<"] : "<<((x)==(y)?"PASSED":"FAILED")<<vcl_endl;}

int test_sstream_main(int /*argc*/,char* /*argv*/[])
{
  int status = 0;
  vcl_string x = "fred";
  vcl_istringstream ss(x);

  vcl_string fred;
  ss >> fred;
  AssertEq(fred,"fred");

  vcl_istringstream s("wilma");

  char w = '?';
  s >> w; AssertEq((int)w,'w');
  s >> w; AssertEq((int)w,'i');
  s >> w; AssertEq((int)w,'l');
  s >> w; AssertEq((int)w,'m');
  s >> w; AssertEq((int)w,'a');

  return status;
}
