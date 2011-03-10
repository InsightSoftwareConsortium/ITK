// This is vcl/vcl_deprecated.cxx
#include "vcl_deprecated.h"

#include <vcl_iostream.h>
#include <vcl_cstdlib.h>

void
vcl_deprecated_warn( const char* func_name )
{
  vcl_cerr << "Function " << func_name << " is deprecated." << vcl_endl;
}

void
vcl_deprecated_abort( const char* func_name )
{
  vcl_cerr << "Function " << func_name << " is deprecated." << vcl_endl;
  vcl_abort();
}
