#include <vcl_iostream.h>
#include <vcl_iterator.txx>

#if !defined(VCL_SGI_CC) && !defined(WIN32) && !defined(GNU_LIBSTDCXX_V3) 
template class vcl_ostream_iterator<int>;
#endif
