// This is vcl/vcl_deprecated.cxx
#include <iostream>
#include <cstdlib>
#include "vcl_deprecated.h"



#ifdef VXL_WARN_DEPRECATED_ABORT

void
vcl_deprecated_abort( const char * func_name )
{
  std::cerr << "Function " << func_name << " is deprecated." << std::endl;
  std::abort();
}

#else

void
vcl_deprecated_warn( const char* func_name )
{
  std::cerr << "Function " << func_name << " is deprecated." << std::endl;
}

#endif
