#include <vcl_string.h>
#include <vcl_map.txx>

#ifndef VCL_BROKEN_AS
VCL_MULTIMAP_INSTANTIATE(int, vcl_string, vcl_less<int>);
VCL_MAP_INSTANTIATE(int, vcl_string, vcl_less<int>);
#endif
