#include <vcl_compiler.h>
#if defined(VCL_SGI_CC) && VCL_USE_NATIVE_STL
#include <alloc.h>
template class std::__default_alloc_template<true,0>;
template class std::__malloc_alloc_template<0>;
#endif
