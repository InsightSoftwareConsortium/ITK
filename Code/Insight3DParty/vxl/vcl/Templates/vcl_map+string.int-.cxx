#include <vcl_string.h>
#include <vcl_map.txx>

#ifndef VCL_BROKEN_AS
VCL_MULTIMAP_INSTANTIATE(vcl_string, int, vcl_less<vcl_string>);
VCL_MAP_INSTANTIATE(vcl_string, int, vcl_less<vcl_string>);
#endif
