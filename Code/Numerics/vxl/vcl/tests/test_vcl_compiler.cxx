#include <vcl_iostream.h>

int main()
{
  for (int i = 0; i < 2; ++i)
    vcl_cout << i << vcl_endl;
  for (int i = 0; i < 2; ++i)
    ;

  return 0;
}
