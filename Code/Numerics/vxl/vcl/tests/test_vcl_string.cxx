#include <vcl_string.h> // C++ specific includes first
#include <vcl_cassert.h>
#include <vcl_iostream.h>

#define Assert(x) {vcl_cout << "TEST " #x " : "; vcl_cout << ((x)?"PASSED":"FAILED")}

#define AssertEq(x) {vcl_cout<<"TEST ["<<fred<<"] == ["<<x<<"] : ";vcl_cout<<(fred==(x)?"PASSED":"FAILED")<<vcl_endl;}

int main()
{
  vcl_string fred;
  fred = "fred";

  AssertEq("fred");

  fred += ", una";
  AssertEq("fred, una");

  fred.replace(3,1, "on");
  AssertEq("freon, una");

  fred.erase(5, 2);
  AssertEq("freonuna");

  fred.erase(5);
  AssertEq("freon");

  return 0;
}
