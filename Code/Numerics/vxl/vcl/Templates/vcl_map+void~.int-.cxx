#include <vcl_map.txx>

#if !VCL_USE_NATIVE_STL || !defined(VCL_SGI_CC)
VCL_MULTIMAP_INSTANTIATE(void*, int, vcl_less<void*>);
VCL_MAP_INSTANTIATE(void*, int, vcl_less<void*>);
#endif
