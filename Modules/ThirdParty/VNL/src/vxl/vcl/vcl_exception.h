#ifndef vcl_exception_h_
#define vcl_exception_h_

#include <exception>
#include "vcl_compiler.h"

# define vcl_throw     throw
# define vcl_try       try
# define vcl_catch     catch
# define vcl_catch_all catch (...)

//vcl alias names to std names
#define vcl_exception std::exception
#define vcl_bad_exception std::bad_exception

#endif // vcl_exception_h_
