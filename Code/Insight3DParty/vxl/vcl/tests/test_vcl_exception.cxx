
#define abort my_abort
void my_abort();

#include <vcl_exception.h>

#include <vcl_iostream.h>

void my_abort()
{
  vcl_cerr << "abort()\n";
}

main()
{
  vcl_try {
    vcl_throw "Bad something....\n";
  }
  vcl_catch (char* e) {
    vcl_cerr << "caught " << e << vcl_endl;
  }
  vcl_catch_all {
    vcl_cerr << "caught nuffink " << vcl_endl;
  }
}
