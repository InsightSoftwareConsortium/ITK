#include <vcl_functional.h>
#include <vcl_string.h>
#include <vcl_set.txx>

#ifndef VCL_BROKEN_AS
VCL_SET_INSTANTIATE(vcl_string, vcl_less<vcl_string> );
#endif
